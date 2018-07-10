package com.bones

import cats.effect.IO
import com.bones.data.Algebra.{DataDefinitionOp, StringData}
import com.bones.rest.Algebra.{Create, Read}
import com.bones.rest.unfiltered.{DirectToDoobie, UnfilteredUtil}
import com.bones.rest.unfiltered.DirectToDoobie.DoobieInfo
import doobie.{Transactor, Update0}
import doobie.util.transactor.Transactor.Aux
import unfiltered.filter.Planify
import doobie._
import doobie.implicits._
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{ValidationOp, IntValidation => iv, StringValidation => sv}
import cats._
import cats.implicits._
import unfiltered.jetty


object PersonEndpoint extends App {

  case class Person(name: String, age: Int)

  val personSchema = obj2(
    key("name").string(sv.alphanum()),
    key("age").int(iv.min(0))
  ).transform[Person]

  val personwithId =
    obj1(key("id").int()) :: personSchema

  val errorDef: DataDefinitionOp[String] = StringData()

  val doobieInfo = new DoobieInfo[Person] {
    override def transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
    )

    override def get(id: Long): doobie.ConnectionIO[Person] =
      sql"select name, age from person where id = $id".query[Person].unique

    override def insert(a: Person): doobie.ConnectionIO[Int] = {
      val name = a.name
      val age = a.age
      sql"insert into person (name, age) values ($name, $age)".update.run
    }
  }

  val serviceDescription = new Create[Person, String, Person] with Read[Person] {
    override def schemaForCreate: DataDefinitionOp[Person] = personSchema
    override def successSchemaForCreate: DataDefinitionOp[Person] = personSchema
    override def errorSchemaForCreate: DataDefinitionOp[String] = errorDef
    override def successSchemaForRead: DataDefinitionOp[Person] = personSchema
  }

  val restToDoobieInterpreter = DirectToDoobie("/person")

  val servletDefinitions = restToDoobieInterpreter.apply(serviceDescription).apply(doobieInfo)

  val plan = UnfilteredUtil.toPlan(servletDefinitions)

  jetty.Http(5678).filter(new Planify(plan)).run

}
