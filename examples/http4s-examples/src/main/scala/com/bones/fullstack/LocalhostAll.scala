package com.bones.fullstack


import java.nio.charset.StandardCharsets

import cats.data.{Kleisli, NonEmptyList}
import cats.effect._
import cats.implicits._
import com.bones.data.Error.ExtractionError
import com.bones.data.{BonesSchema, KvpNil}
import com.bones.http4s.ClassicCrudInterpreter
import com.bones.jdbc.{DbColumnInterpreter, DbUtil}
import com.bones.react.{CreateReactFile, CreateReactFiles}
import com.bones.syntax.{kvp, long, lv}
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.{Header, HttpRoutes}

import scala.util.Try

object LocalhostAllIOApp {

  case class BasicError(message: String)
  private val basicErrorSchema =
    (kvp("message", com.bones.syntax.string) :: KvpNil).convert[BasicError]

  def extractionErrorToBasicError(extractionError: ExtractionError) : BasicError = {
    BasicError(extractionError.toString)
  }
  def extractionErrorsToBasicError(extractionErrors: NonEmptyList[ExtractionError]): BasicError = {
    BasicError(extractionErrors.toList.mkString("."))
  }


  def localhostDataSource: HikariDataSource = {
    val config = new HikariConfig
    config.setJdbcUrl("jdbc:postgresql:bones")
    config.setDriverClassName("org.postgresql.Driver")
    config.setUsername("tstevens")
    config.setPassword("")
    config.addDataSourceProperty("cachePrepStmts", "true")
    config.addDataSourceProperty("prepStmtCacheSize", "250")
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    new HikariDataSource(config)
  }

  def dbSchemaEndpoint[A](path: String, schema: BonesSchema[A]): HttpRoutes[IO] = {
      val dbSchema = DbColumnInterpreter.tableDefinition(schema)
      HttpRoutes.of[IO] {
        case GET -> Root / "dbSchema" / path => Ok(dbSchema, Header("Content-Type", "text/plain"))
      }
  }

  def reactEndpoints(schemas: List[BonesSchema[_]]): HttpRoutes[IO] = {
    val (indexJs, indexHtml, library) = CreateReactFiles.fromSchemas(schemas)
    HttpRoutes.of[IO] {
      case GET -> Root / "webapp" / "index.html" => Ok(indexHtml, Header("Content-Type", "text/html"))
      case GET -> Root / "webapp" / "index.js" => Ok(library + "\n" + indexJs, Header("Content-Type", "text/javascript"))
    }
  }

  def serviceRoutesWithCrudMiddleware[A](path: String, schema: BonesSchema[A],ds: DataSource
                                                                             ): HttpRoutes[IO] = {


    val middleware = CrudDbDefinitions(schema, ds)


    /** Create the REST interpreter with full CRUD capabilities which write data to the database */
    val interpreter = ClassicCrudInterpreter.allVerbs[A,BasicError,IO,Long](
      path,
      StandardCharsets.UTF_8,
      schema,
      kvp("id", long(lv.min(1))),
      str => Try(str.toLong).toEither.left.map(ex => BasicError("Could not convert parameter to a Long value")),
      basicErrorSchema,
      Kleisli(middleware.createF).map(_.left.map(e => extractionErrorToBasicError(e))).run,
      Kleisli(middleware.readF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
      Function.untupled(Kleisli(middleware.updateF.tupled).map(_.left.map(e => extractionErrorsToBasicError(e))).run),
      Kleisli(middleware.deleteF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
      () => middleware.searchF
    )

    val dbRoutes = dbSchemaEndpoint(path, schema)

    interpreter.createRoutes <+> dbRoutes

  }
}

abstract class LocalhostAllIOApp() extends IOApp {

  def services: HttpRoutes[IO]

  override def run(args: List[String]): IO[ExitCode] = {

    val allServices = services


    BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(CORS(allServices), "/")
      .serve
      .compile.drain.as(ExitCode.Success)

  }

}



