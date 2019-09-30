package com.bones

import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import cats.effect.IO
import com.bones.data.Value.KvpNil
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{LongValidation => iv, StringValidation => sv}
import com.zaxxer.hikari.HikariDataSource
import org.http4s.HttpRoutes


object Definitions {

  case class Person(name: String, age: Long, gender: Option[String])

  val personSchema = (
    kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
      kvp("age", long(iv.min(0))) ::
      kvp("gender", string.optional) ::
      KvpNil
    ).convert[Person]

}


object PersonDoc extends App {
  val info = new Info()
    .description("Test Person Endpoint")
    .title("Person")
    .version("1.0")

  val openApi = new OpenAPI()

}

object PersonEndpoint extends LocalhostAllIOApp {

  import LocalhostAllIOApp._

  val ds: HikariDataSource = localhostDataSource

  override def services: HttpRoutes[IO] =
    serviceRoutesWithCrudMiddleware("person", Definitions.personSchema, ds)
}
