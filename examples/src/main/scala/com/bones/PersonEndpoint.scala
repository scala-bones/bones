package com.bones

import cats.effect.IO
import com.bones.crud.Algebra._
import com.bones.data.Value.{KvpNil, StringData, ValueDefinitionOp}
import com.bones.oas3.CrudOasInterpreter
import com.bones.rest.unfiltered.Orm.Dao
import com.bones.rest.unfiltered.UnfilteredUtil
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}
import doobie.Transactor
import doobie.util.transactor.Transactor.Aux
import io.swagger.parser.{Swagger20Parser, SwaggerParser}
import unfiltered.filter.Planify


object Interpreter {

  def doInterpretation[A:Manifest,B](serviceDescription: List[CrudOp[A]], doobieInfo: Dao[A], transactor: TransactorF, endpoint: String, errorDef: ValueDefinitionOp[B]): Unit = {

    val restToDoobieInterpreter = DirectToDoobie(endpoint)

    val servletDefinitions = restToDoobieInterpreter.apply(serviceDescription).apply(doobieInfo).apply(transactor)

    println("servlet defs: " + servletDefinitions)

    val plan = UnfilteredUtil.toPlan(servletDefinitions)

    val oas = CrudOasInterpreter().apply(serviceDescription).apply(endpoint).spaces2

//    println(oas)

    new Swagger20Parser().parse(oas)
    val swagger = new SwaggerParser().parse(oas)
    val swaggerPretty = io.swagger.util.Json.pretty(swagger)
//    println(swaggerPretty)

//    println(DoobiePostgresSchema("person").apply(serviceDescription))

    unfiltered.jetty.Server.http(5678).plan(new Planify(plan)).run

  }

}
object PersonEndpoint extends App {

  case class Person(name: String, age: Int)

  object Person {
    implicit val dao: Dao.Aux[Person, Int] =
      Dao.derive[Person, Int]("person", "id")
  }

  val personSchema = (
    key("name").string(sv.matchesRegex("^[a-zA-Z ]*$".r)) ::
    key("age").int(iv.min(0)) ::
    KvpNil
  ).transform[Person]

  val errorDef: ValueDefinitionOp[String] = StringData()

  val serviceDescription =
    create(personSchema, errorDef, personSchema) ::
    read(personSchema) ::
    update(personSchema, errorDef, personSchema) ::
    delete(personSchema) ::
    Nil

  //Above is the description.
  //below interpreters the description into runnable code.


  val transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
  )



  Interpreter.doInterpretation(serviceDescription, Person.dao, () => transactor, "/person", errorDef)



}
