package com.bones.fullstack

import java.nio.charset.StandardCharsets

import cats.data.{Kleisli, NonEmptyList}
import cats.effect._
import cats.implicits._
import com.bones.Util
import com.bones.data.BonesSchema
import com.bones.data.Error.ExtractionError
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.http4s.ClassicCrudInterpreter
import com.bones.jdbc.DbColumnInterpreter
import com.bones.syntax._
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.{Header, HttpRoutes}

object LocalhostAllIOApp {

  implicit val dsl = org.http4s.dsl.io

  case class BasicError(message: String)
  private val basicErrorSchema =
    (("message", com.bones.syntax.string) :<: kvpNil).convert[BasicError]

  def extractionErrorToBasicError(extractionError: ExtractionError): BasicError = {
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

  def dbSchemaEndpoint[A](path: String, schema: BonesSchema[NoAlgebra, A]): HttpRoutes[IO] = {
    val dbSchema = DbColumnInterpreter.tableDefinition(schema)
    HttpRoutes.of[IO] {
      case GET -> Root / "dbSchema" / path => Ok(dbSchema, Header("Content-Type", "text/plain"))
    }
  }

  def serviceRoutesWithCrudMiddleware[A](
    path: String,
    schema: BonesSchema[NoAlgebra, A],
    ds: DataSource): HttpRoutes[IO] = {

    val middleware = CrudDbDefinitions(schema, ds)

    /** Create the REST interpreter with full CRUD capabilities which write data to the database */
    val interpreter = ClassicCrudInterpreter.allVerbs[A, BasicError, IO, Long](
      path,
      schema,
      kvp("id", long(lv.min(1))),
      str =>
        Util
          .stringToLong(str)
          .toRight(StringToIdError(str, "Could not convert parameter to a Long value")),
      basicErrorSchema,
      Kleisli(middleware.createF).map(_.left.map(e => extractionErrorToBasicError(e))).run,
      Kleisli(middleware.readF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
      Function.untupled(
        Kleisli(middleware.updateF.tupled)
          .map(_.left.map(e => extractionErrorsToBasicError(e)))
          .run),
      Kleisli(middleware.deleteF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
      () => middleware.searchF,
      StandardCharsets.UTF_8
    )
    val dbRoutes = dbSchemaEndpoint(path, schema)
    interpreter.createRoutes <+> dbRoutes

  }
}

abstract class LocalhostAllIOApp() extends IOApp {

  def services: HttpRoutes[IO]

  override def run(args: List[String]): IO[ExitCode] = {

    val allServices = services

    BlazeBuilder[IO]
      .bindHttp(8080, "localhost")
      .mountService(CORS(allServices), "/")
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  }

}
