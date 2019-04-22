package com.bones.fullstack


import com.bones.Definitions
import com.bones.data.Value.BonesSchema
import com.bones.http4s.HttpInterpreter

import cats.effect._
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.syntax._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze._

abstract class LocalhostAllIOApp[A](postgresSchema: String, schema: BonesSchema[A]) extends IOApp {

  import Definitions._

  import com.zaxxer.hikari.HikariConfig
  import com.zaxxer.hikari.HikariDataSource
  import java.sql.SQLException

  val config = new HikariConfig
  config.setJdbcUrl("jdbc:postgresql:bones")
  config.setDriverClassName("org.postgresql.Driver")
  config.setUsername("postgres")
  config.setPassword("")
  config.addDataSourceProperty("cachePrepStmts", "true")
  config.addDataSourceProperty("prepStmtCacheSize", "250")
  config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")
  val ds = new HikariDataSource(config)

  val middleware = CrudDbDefinitions(schema, ds)

  override def run(args: List[String]): IO[ExitCode] = {


    val http4Service = HttpInterpreter().forService(
      personService,
      middleware.createF,
      middleware.readF,
      middleware.searchF,
      middleware.updateF,
      middleware.deleteF
    )

    BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/")
      .serve
      .compile.drain.as(ExitCode.Success)

  }

}



