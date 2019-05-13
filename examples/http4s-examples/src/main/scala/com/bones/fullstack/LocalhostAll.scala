package com.bones.fullstack


import cats.effect._
import cats.implicits._
import com.bones.crud.Algebra.ServiceOps
import com.bones.crud.WithId
import com.bones.data.Value.BonesSchema
import com.bones.fullstack.CrudDbDefinitions.DbError
import com.bones.http4s.HttpInterpreter
import com.bones.jdbc.{DbColumnInterpreter, DbUtil}
import com.bones.react.{CreateReactFile, CreateReactFiles}
import com.bones.syntax.{kvp, long, lv}
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import org.http4s.server.middleware.CORS
import org.http4s.{Header, HttpRoutes}

object LocalhostAllIOApp {

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

  def dbSchemaEndpoint(serviceOps: ServiceOps[_, _, _, _, _, _, _, _, _, _]): HttpRoutes[IO] = {
    serviceOps.createOperation.map(op => {
      val dbSchema = DbColumnInterpreter.tableDefinition(op.inputSchema)
      HttpRoutes.of[IO] {
        case GET -> Root / "dbSchema" / serviceOps.path => Ok(dbSchema, Header("Content-Type", "text/plain"))
      }
    }).getOrElse(HttpRoutes.empty)
  }

  def reactEndpoints(schemas: List[BonesSchema[_]]): HttpRoutes[IO] = {
    val (indexJs, indexHtml, library) = CreateReactFiles.fromSchemas(schemas)
    HttpRoutes.of[IO] {
      case GET -> Root / "webapp" / "index.html" => Ok(indexHtml, Header("Content-Type", "text/html"))
      case GET -> Root / "webapp" / "index.js" => Ok(library + "\n" + indexJs, Header("Content-Type", "text/javascript"))
    }
  }

  def serviceRoutesWithCrudMiddleware[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
                                                                               serviceOp: ServiceOps[CI, CI, DbError, RO, DbError, UI, UI, DbError, DO, DbError],
                                                                               ds: DataSource
                                                                             ): HttpRoutes[IO] = {
    val createOperationWitId = serviceOp.createOperation.map(c => c.copy(outputSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, c.outputSchema)))
    val readOperationWithId = serviceOp.readOperation.map(r => r.copy(outputSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, r.outputSchema)))
    val updateOperationWithId = serviceOp.updateOperation.map(u => u.copy(outputSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, u.outputSchema)))
    val deleteOperationWithid = serviceOp.deleteOperation.map(d => d.copy(outputSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, d.outputSchema)))
    val serviceOpWithId =
      ServiceOps(serviceOp.path, createOperationWitId, readOperationWithId, updateOperationWithId, deleteOperationWithid)

    val middleware = CrudDbDefinitions(serviceOp, ds)


    val interpreterRoutes = HttpInterpreter().forService[IO, CI, WithId[Long, CI], DbError, WithId[Long, RO], DbError, UI, WithId[Long, UI], DbError, WithId[Long, DO], DbError](
      serviceOpWithId,
      middleware.createF,
      middleware.readF,
      middleware.searchF,
      middleware.updateF,
      middleware.deleteF
    )
    val dbRoutes = dbSchemaEndpoint(serviceOp)

    interpreterRoutes <+> dbRoutes

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



