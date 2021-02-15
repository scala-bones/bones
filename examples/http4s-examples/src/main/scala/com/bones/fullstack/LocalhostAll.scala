package com.bones.fullstack

import cats.effect._
import cats.implicits._
import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common.{ClassicCrudDef, StringToIdError}
import com.bones.http4s.ClassicCrud
import com.bones.http.common.StringToIdError
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import com.bones.jdbc.insert.DbInsert
import com.bones.jdbc.select.SelectInterpreter
import com.bones.jdbc.update.DbUpdate
import com.bones.jdbc.{DbDelete, DbSearch}
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}

import javax.sql.DataSource
import org.http4s.dsl.io._
import org.http4s.headers.`Content-Type`
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.{Header, HttpRoutes}

import scala.concurrent.ExecutionContext.Implicits.global

object LocalhostAllIOApp {

  implicit val dsl = org.http4s.dsl.io

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

  def dbSchemaEndpoint[A](
    path: String,
    schema: KvpCollection[String, DefaultValues, A]): HttpRoutes[IO] = {
    val dbSchema =
      com.bones.jdbc.column.defaultDbColumnInterpreter.tableDefinitionCustomAlgebra(schema)
    HttpRoutes.of[IO] {
      case GET -> Root / "dbSchema" / p if p == path =>
        Ok(dbSchema, Header("Content-Type", "text/plain"))
    }
  }

  def toErrorResponse[A, ID](io: IO[Either[ExtractionErrors[String], (ID, A)]]) =
    io.map(_.left.map(ErrorResponse))

  def serviceRoutesWithCrudMiddleware[A: Manifest, ID: Manifest](
    classicCrudDef: ClassicCrudDef[
      DefaultValues,
      A,
      ID,
      `Content-Type`,
      ErrorResponse,
      StringToIdError],
    dbGet: SelectInterpreter[DefaultValues],
    dbSearch: DbSearch[DefaultValues],
    dbInsert: DbInsert[DefaultValues],
    dbUpdate: DbUpdate[DefaultValues],
    dbDelete: DbDelete[DefaultValues],
    ds: DataSource): HttpRoutes[IO] = {

    val middleware =
      CrudDbDefinitions[DefaultValues, A, ID](
        classicCrudDef.schema,
        classicCrudDef.idSchema,
        dbGet,
        dbSearch,
        dbInsert,
        dbUpdate,
        dbDelete,
        ds)

    /*
com.bones.httpcommon.ClassicCrudDef[com.bones.data.values.DefaultValues,A(in method serviceRoutesWithCrudMiddleware),ID,String,com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse,com.bones.httpcommon.StringToIdError]
com.bones.httpcommon.ClassicCrudDef[[A(in value <local DefaultValues>)]com.bones.data.values.DefaultValues[A(in value <local DefaultValues>)],Any,ID,String,com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse,com.bones.httpcommon.StringToIdError]
[error]     (which expands to)  com.bones.httpcommon.ClassicCrudDef[[A(in value <local DefaultValues>)]com.bones.data.values.ScalaCoreValue[A(in value <local DefaultValues>)] :+: com.bones.data.values.CustomStringValue[A(in value <local DefaultValues>)] :+: com.bones.data.values.JavaTimeValue[A(in value <local DefaultValues>)] :+: com.bones.data.values.JavaUtilValue[A(in value <local DefaultValues>)] :+: shapeless.CNil,Any,ID,String,com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse,com.bones.httpcommon.StringToIdError]
     */
    /** Create the REST interpreter with full CRUD capabilities which write data to the database */
    val restRoutes =
      ClassicCrud.get(classicCrudDef, middleware.readF) <+>
        ClassicCrud.put(classicCrudDef, middleware.updateF) <+>
        ClassicCrud.post(classicCrudDef, middleware.createF) <+>
        ClassicCrud.delete(classicCrudDef, middleware.deleteF)

    val dbRoutes = dbSchemaEndpoint(classicCrudDef.path, classicCrudDef.schema)
    restRoutes <+> dbRoutes

  }
}

abstract class LocalhostAllIOApp() extends IOApp {

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
