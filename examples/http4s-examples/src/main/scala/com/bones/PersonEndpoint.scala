package com.bones

import _root_.io.swagger.v3.oas.models.OpenAPI
import _root_.io.swagger.v3.oas.models.info.Info
import cats.effect.IO
import com.bones.crud.Algebra._
import com.bones.data.Value.KvpNil
import com.bones.fullstack.CrudDbDefinitions.DbError
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{LongValidation => iv, StringValidation => sv}
import org.http4s.HttpRoutes




object Definitions {

  case class Person(name: String, age: Long, gender: Option[String])

  val personSchema = (
      kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
      kvp("age", long(iv.min(0))) ::
      kvp("gender", string.optional) ::
      KvpNil
    ).convert[Person]

  val errorDef = (kvp("error", string) :: KvpNil).convert[DbError]

  val personService = ServiceOps.withPath("person")
    .withCreate(personSchema, personSchema, errorDef)
    .withRead(personSchema, errorDef)
    .withUpdate(personSchema, personSchema, errorDef)
    .withDelete(personSchema, errorDef)

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

object PersonEndpoint extends LocalhostAllIOApp {

  import LocalhostAllIOApp._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] =
    serviceRoutesWithCrudMiddleware(Definitions.personService, ds)
}
