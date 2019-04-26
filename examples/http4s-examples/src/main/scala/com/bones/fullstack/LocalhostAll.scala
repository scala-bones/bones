package com.bones.fullstack


import cats.effect._
import cats.implicits._
import com.bones.crud.Algebra.ServiceOps
import com.bones.crud.WithId
import com.bones.fullstack.CrudDbDefinitions.DbError
import com.bones.http4s.HttpInterpreter
import com.bones.jdbc.{DbColumnInterpreter, DbUtil}
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

  def dbSchemaEndpoint(serviceOps: ServiceOps[_,_,_,_,_,_,_,_,_,_]): HttpRoutes[IO] = {
    serviceOps.createOperation.map(op => {
      val dbSchema = DbColumnInterpreter.tableDefinition(op.inputSchema)
      HttpRoutes.of[IO] {
        case GET -> Root / "dbSchema" / serviceOps.path => Ok(dbSchema, Header("Content-Type", "text/html"))
      }
    }).getOrElse(HttpRoutes.empty)
  }

  def serviceRoutesWithCrudMiddleware[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
    serviceOp: ServiceOps[CI, CI, DbError, RO, DbError, UI, UI, DbError, DO, DbError],
    ds: DataSource
  ): HttpRoutes[IO] = {
    val createOperationWitId = serviceOp.createOperation.map(c => c.copy(successSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, c.successSchema)))
    val readOperationWithId = serviceOp.readOperation.map(r => r.copy(successSchemaForRead = WithId.entityWithId(DbUtil.longIdKeyValueDef, r.successSchemaForRead)))
    val updateOperationWithId = serviceOp.updateOperation.map(u => u.copy(successSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, u.successSchema)))
    val deleteOperationWithid = serviceOp.deleteOperation.map(d => d.copy(successSchema = WithId.entityWithId(DbUtil.longIdKeyValueDef, d.successSchema)))
    val serviceOpWithId =
      ServiceOps(serviceOp.path, createOperationWitId, readOperationWithId, updateOperationWithId, deleteOperationWithid)

    val middleware = CrudDbDefinitions(serviceOp, ds)


    val interpreterRoutes = HttpInterpreter().forService[CI, WithId[Long,CI], DbError, WithId[Long,RO], DbError, UI, WithId[Long,UI], DbError, WithId[Long,DO], DbError](
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



