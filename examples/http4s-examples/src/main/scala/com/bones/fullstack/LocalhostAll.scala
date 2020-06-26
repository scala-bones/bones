package com.bones.fullstack

import java.nio.charset.StandardCharsets

import cats.data.{Kleisli, NonEmptyList}
import cats.effect._
import cats.implicits._
import com.bones.Util
import com.bones.data.{BonesSchema, KvpNil, KvpSingleValueHead}
import com.bones.data.Error.ExtractionError
import com.bones.data.custom.AllCustomAlgebras
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.http4s.ClassicCrudInterpreter
import com.bones.jdbc.{IdDefinition, JdbcColumnInterpreter}
import com.bones.jdbc.column.DbColumnInterpreter
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
    (("message", com.bones.syntax.string) :: new KvpNil[AllCustomAlgebras]).convert[BasicError]

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

  def dbSchemaEndpoint[A](path: String, schema: BonesSchema[AllCustomAlgebras, A]): HttpRoutes[IO] = {
    val dbSchema = DbColumnInterpreter.tableDefinitionCustomAlgebra(schema, com.bones.jdbc.column.defaultDbColumnInterpreter)
    HttpRoutes.of[IO] {
      case GET -> Root / "dbSchema" / p if p == path => Ok(dbSchema, Header("Content-Type", "text/plain"))
    }
  }

  def serviceRoutesWithCrudMiddleware[ALG[_], A, ID:Manifest](
    path: String,
    schema: BonesSchema[AllCustomAlgebras, A],
    idDef: IdDefinition[AllCustomAlgebras, ID],
    parseIdF: String => Either[StringToIdError,ID],
    jdbcColumnInterpreter: JdbcColumnInterpreter[AllCustomAlgebras],
    ds: DataSource): HttpRoutes[IO] = {

    val middleware = CrudDbDefinitions[AllCustomAlgebras, A, ID](schema, jdbcColumnInterpreter, idDef, ds)

    /** Create the REST interpreter with full CRUD capabilities which write data to the database */
    val interpreter = ClassicCrudInterpreter.allVerbsCustomAlgebra[AllCustomAlgebras, A, BasicError, IO, ID](
      path,
      com.bones.circe.custom.allValidators,
      com.bones.circe.custom.allEncoders,
      com.bones.bson.custom.allValidators,
      com.bones.bson.custom.allEncoders,
      com.bones.protobuf.custom.allValidators,
      com.bones.protobuf.custom.allEncoders,
      com.bones.protobuf.custom.allProtoFiles,
      com.bones.swagger.custom.allInterpreters,
      schema,
      idDef.value,
      parseIdF,
      basicErrorSchema,
      Kleisli(middleware.createF).map(_.left.map(e => extractionErrorsToBasicError(e))).run,
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

  val idDef = IdDefinition("id", long(lv.positive))
  val parseIdF: String => Either[StringToIdError,Long] =
    str => Util.stringToLong(str).toRight(StringToIdError(str, "Could not convert to Long"))

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
