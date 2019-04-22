package com.bones

import java.sql.Connection

import _root_.io.swagger.v3.oas.models.OpenAPI
import _root_.io.swagger.v3.oas.models.info.Info
import cats.effect._
import cats.implicits._
import com.bones.crud.Algebra._
import com.bones.data.Value.KvpNil
import com.bones.fullstack.CrudDbDefinitions
import com.bones.http4s.HttpInterpreter
import com.bones.jdbc.{DbDelete, DbGet, DbInsertValues, DbUpdateValues}
import com.bones.oas3.CrudOasInterpreter
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{LongValidation => iv, StringValidation => sv}
import org.http4s.server.blaze._
import fs2.Stream
import javax.sql.DataSource




object Definitions {

  case class Person(name: String, age: Long, gender: Option[String])
  case class WithId[A](id: Long, a: A)


  val personSchema = (
      kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
      kvp("age", long(iv.min(0))) ::
      kvp("gender", string.optional) ::
      KvpNil
    ).convert[Person]

  val personWithId =
    (kvp("id", long) :: personSchema :><: KvpNil).convert[WithId[Person]]

  case class Error(error: String)

  val errorDef = (kvp("error", string) :: KvpNil).convert[Error]

  val personService = ServiceOps.withPath("person")
    .withCreate(personSchema, personWithId, errorDef)
    .withRead(personWithId, errorDef)
    .withUpdate(personSchema, personWithId, errorDef)
    .withDelete(personWithId, errorDef)

  //Above is the description.
  //below interpreters the description into runnable code.


}




object PersonDoc extends App {
  val info = new Info()
    .description("Test Person Endpoint")
    .title("Person")
    .version("1.0")

  val openApi = new OpenAPI()

//  CrudOasInterpreter.jsonApiForService(Definitions.personService).apply(openApi)


//  println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi))
}

object PersonEndpoint extends IOApp {
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



  override def run(args: List[String]): IO[ExitCode] = {


      val middleware = CrudDbDefinitions(personSchema, ds)
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
