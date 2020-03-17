package com.bones

import cats.effect.IO
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.fullstack.LocalhostAllIOApp._
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}
import com.zaxxer.hikari.HikariDataSource
import org.http4s.HttpRoutes

/** Example endpoint.  This creates a complete application which saves a person to a local database including:
  * JSON endpoints, Protobuf Endpoints, 5 CRUD Endpoints (Get, Put, Post, Delete, Search AllCustomAlgebras),
  * Swagger, DB DDL.
  */
object PersonEndpoint extends LocalhostAllIOApp {

  val ds: HikariDataSource = localhostDataSource

  case class Person(name: String, age: Int, gender: Option[String])

  val personSchema = (
    ("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r)), "The name of the person must be alphanumeric", "John Doe") :<:
      ("age", int(iv.min(0)), "The Age, In years, of the person.", 21) :<:
      ("gender", string.optional) :<:
      kvpNil
  ).convert[Person]

  override def services: HttpRoutes[IO] =
    serviceRoutesWithCrudMiddleware("person", personSchema, ds)
}
