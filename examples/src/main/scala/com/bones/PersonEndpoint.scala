package com.bones

import cats.effect.IO
import com.bones.crud.Algebra._
import com.bones.data.Value.{KvpNil, StringData, ValueDefinitionOp}
import com.bones.http4s.Orm.Dao
import com.bones.oas3.CrudOasInterpreter
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}
import doobie.Transactor
import doobie.util.transactor.Transactor.Aux
import io.swagger.parser.{Swagger20Parser, SwaggerParser}
import org.http4s.server.blaze.BlazeBuilder
import unfiltered.filter.Planify
import shapeless._
import cats.implicits._
import com.bones.http4s.HttpInterpreter
import fs2.{Stream, StreamApp}
import fs2.StreamApp.ExitCode
import io.swagger.v3.oas.models.info.Info
import org.http4s.HttpService
import org.http4s.server.blaze._

object Interpreter {

  def doInterpretation[A:Manifest,B](serviceDescription: List[CrudOp[A]], doobieInfo: Dao.Aux[A,Int], transactor: Transactor.Aux[IO,Unit], rootDir: String, errorDef: ValueDefinitionOp[B]): HttpService[IO] = {

    import com.bones.http4s.Algebra._
    val service =
      HttpInterpreter(rootDir)
      .withContentType(jsonFormat)
      .withSwagger()


    service.saveWithDoobieInterpreter[A](serviceDescription, doobieInfo, transactor)


  }

}
object Definitions {

  case class Person(name: String, age: Int)

  object Person {
    implicit val dao: Dao.Aux[Person, Int] =
      Dao.derive[Person, Int]("person", "id")
  }

  val personSchema = (
    key("name").string(sv.matchesRegex("^[a-zA-Z ]*$".r)) ::
      key("age").int(iv.min(0)) ::
      KvpNil
    ).convert[Person]

  //  val personWithIdSchema = (
  //    key("id").int() :: personSchema ::: KvpNil
  //  ).convert[(Int, Person)]

  val errorDef: ValueDefinitionOp[String] = StringData()

  val serviceDescription =
    create(personSchema, errorDef, personSchema) ::
      read(personSchema) ::
      update(personSchema, errorDef, personSchema) ::
      delete(personSchema) ::
      Nil

  //Above is the description.
  //below interpreters the description into runnable code.
}

object PersonDoc extends App {
  val info = new Info()
    .description("Test Person Endpoint")
    .title("Person")
    .version("1.0")
  val api = CrudOasInterpreter().toSwaggerCore(Definitions.serviceDescription, "person")
  api.info(info)
  println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(api))
}

object PersonEndpoint extends StreamApp[IO] {
  import Definitions._

  val transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
  )

  import scala.concurrent.ExecutionContext.Implicits._

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    val http4Service = Interpreter.doInterpretation[Person, String](serviceDescription, Person.dao, transactor, "/person", errorDef)
    BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/").serve

  }

}
