package com.bones.fullstack

import java.nio.charset.StandardCharsets

import cats.data.{Kleisli, NonEmptyList}
import cats.effect._
import cats.implicits._
import com.bones.Util
import com.bones.data.Error.ExtractionError
import com.bones.data.values.DefaultValues
import com.bones.data.{KvpCollection, KvpNil}
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.http4s.ClassicCrudInterpreter
import com.bones.http4s.config.InterpreterConfig
import com.bones.jdbc.insert.DbInsert
import com.bones.jdbc.update.DbUpdate
import com.bones.jdbc.{DbDelete, DbGetInterpreter, DbSearch, IdDefinition}
import com.bones.syntax._
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.{Header, HttpRoutes}

import scala.concurrent.ExecutionContext.Implicits.global

object LocalhostAllIOApp {

  implicit val dsl = org.http4s.dsl.io

  case class BasicError(message: String)
  private val basicErrorSchema =
    (("message", com.bones.syntax.string) :: new KvpNil[DefaultValues]).convert[BasicError]

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
    config.setUsername("travis")
    config.setPassword("")
    config.addDataSourceProperty("cachePrepStmts", "true")
    config.addDataSourceProperty("prepStmtCacheSize", "250")
    config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
    new HikariDataSource(config)
  }

  def dbSchemaEndpoint[A](path: String, schema: KvpCollection[DefaultValues, A]): HttpRoutes[IO] = {
    val dbSchema =
      com.bones.jdbc.column.defaultDbColumnInterpreter.tableDefinitionCustomAlgebra(schema)
    HttpRoutes.of[IO] {
      case GET -> Root / "dbSchema" / p if p == path =>
        Ok(dbSchema, Header("Content-Type", "text/plain"))
    }
  }

  def serviceRoutesWithCrudMiddleware[A, ID: Manifest](
    interpreters: InterpreterConfig[DefaultValues, ID],
    path: String,
    schema: KvpCollection[DefaultValues, A],
    parseIdF: String => Either[StringToIdError, ID],
    dbGet: DbGetInterpreter[DefaultValues],
    dbSearch: DbSearch[DefaultValues],
    dbInsert: DbInsert[DefaultValues],
    dbUpdate: DbUpdate[DefaultValues],
    dbDelete: DbDelete[DefaultValues],
    ds: DataSource): HttpRoutes[IO] = {

    val idDef = IdDefinition("id", interpreters.idDefinition)

    val middleware =
      CrudDbDefinitions[DefaultValues, A, ID](
        schema,
        idDef,
        dbGet,
        dbSearch,
        dbInsert,
        dbUpdate,
        dbDelete,
        ds)

    /** Create the REST interpreter with full CRUD capabilities which write data to the database */
    val interpreter =
      ClassicCrudInterpreter.allVerbs[DefaultValues, A, BasicError, IO, ID](
        interpreters,
        path,
        schema,
        parseIdF,
        basicErrorSchema,
        Kleisli(middleware.createF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
        Kleisli(middleware.readF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
        Function.untupled(
          Kleisli(middleware.updateF.tupled)
            .map(_.left.map(e => extractionErrorsToBasicError(e)))
            .run),
        Kleisli(middleware.deleteF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
        () => middleware.searchF
      )
    val dbRoutes = dbSchemaEndpoint(path, schema)
    interpreter.createRoutes <+> dbRoutes

  }
}

abstract class LocalhostAllIOApp() extends IOApp {

  val idDef = IdDefinition("id", long(lv.positive))
  val parseIdF: String => Either[StringToIdError, Long] =
    str => Util.stringToLong(str).toRight(StringToIdError(str, "Could not convert to Long"))

  def services: HttpRoutes[IO]

  override def run(args: List[String]): IO[ExitCode] = {

    val allServices = services

    val router = Router("/" -> CORS(allServices)).orNotFound

    BlazeServerBuilder[IO](global)
      .bindHttp(8080, "localhost")
      .withHttpApp(router)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)

  }

}
