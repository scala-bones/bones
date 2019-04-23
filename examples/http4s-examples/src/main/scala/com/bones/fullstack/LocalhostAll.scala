package com.bones.fullstack


import com.bones.Definitions
import com.bones.data.Value.BonesSchema
import com.bones.http4s.HttpInterpreter
import cats.effect._
import cats.implicits._
import com.bones.crud.Algebra.ServiceOps
import com.bones.fullstack.CrudDbDefinitions.{DbError, WithId}
import com.bones.jdbc.DbColumnInterpreter
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import javax.sql.DataSource
import org.http4s.{Header, HttpRoutes}
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._

object LocalhostAllIOApp {

  def localhostDataSource: HikariDataSource = {
    val config = new HikariConfig
    config.setJdbcUrl("jdbc:postgresql:bones")
    config.setDriverClassName("org.postgresql.Driver")
    config.setUsername("postgres")
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
    serviceOp: ServiceOps[CI, WithId[CI], DbError, RO, DbError, UI, WithId[UI], DbError, DO, DbError],
    ds: DataSource
  ): HttpRoutes[IO] = {
    val middleware = CrudDbDefinitions(serviceOp, ds)
    val interpreterRoutes = HttpInterpreter().forService[CI, WithId[CI], DbError, RO, DbError, UI, WithId[UI], DbError, DO, DbError](
      serviceOp,
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


    BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(services, "/")
      .serve
      .compile.drain.as(ExitCode.Success)

  }

}



