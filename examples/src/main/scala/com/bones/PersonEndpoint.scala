package com.bones

import cats.effect.{IO, IOApp}
import com.bones.crud.Algebra._
import com.bones.data.Value.{DataClass, KvpNil}
import com.bones.http4s.HttpInterpreter
import com.bones.http4s.Orm.Dao
import com.bones.oas3.CrudOasInterpreter
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.util.transactor.Transactor.Aux
import fs2.Stream
import io.swagger.v3.oas.models.info.Info
import org.http4s.HttpService
import org.http4s.server.blaze.BlazeBuilder

object Interpreter {

  def doInterpretation[H,B](serviceDescription: List[CrudOp[H]], doobieInfo: Dao.Aux[H,Int], transactor: HikariTransactor[IO], rootDir: String, errorDef: DataClass[B]): HttpService[IO] = {

    import com.bones.http4s.Algebra._
    val service =
      HttpInterpreter(rootDir)
      .withContentType(jsonFormat)
      .withSwagger()


    service.saveWithDoobieInterpreter[H](serviceDescription, doobieInfo, transactor)


  }

}
object Definitions {

  case class Person(name: String, age: Int)

  object Person {
    implicit val dao: Dao.Aux[Person, Int] =
      Dao.derive[Person, Int]("person", "id")
  }

  val personSchema = (
      kvp("name", string(sv.matchesRegex("^[a-zA-Z ]*$".r))) ::
      kvp("age", int(iv.min(0))) ::
      KvpNil
    ).convert[Person]

  //  val personWithIdSchema = (
  //    key("id").int() :: personSchema ::: KvpNil
  //  ).convert[(Int, Person)]

  case class Error(error: String)

  val errorDef = (kvp("error", string) :: KvpNil).convert[Error]

  val serviceDescription: List[CrudOp[Person]] =
    create(personSchema, personSchema, errorDef) ::
      read(personSchema) ::
      update(personSchema, personSchema, errorDef) ::
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
  val api = CrudOasInterpreter("Person").toSwaggerCore(Definitions.serviceDescription, "person")
  api.info(info)
//  println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(api))
}

object PersonEndpoint extends IOApp {
  import Definitions._

  val transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
  )

  import cats.effect._
  import doobie._
  import doobie.hikari._

  import org.http4s.implicits._
  import org.http4s.dsl.io._
  import org.http4s.server.blaze._
  import cats.effect._
  import cats.implicits._
  import org.http4s.HttpRoutes

  import org.http4s.syntax._

  val hikariTransactor: Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](64) // our connect EC
      te <- ExecutionContexts.cachedThreadPool[IO]    // our transaction EC
      xa <- HikariTransactor.newHikariTransactor[IO](
        "org.postgresql.Driver",                        // driver classname
        "jdbc:postgresql:bones",   // connect URL
        "postgres",                                   // username
        "",                                     // password
        ce,                                     // await connection here
        te                                      // execute JDBC operations here
      )
    } yield xa

  import scala.concurrent.ExecutionContext.Implicits._

  override def run(args: List[String]): IO[ExitCode] = {
    hikariTransactor.use{ xa =>
      val http4Service = Interpreter.doInterpretation[Person, Error](serviceDescription, Person.dao, xa, "/person", errorDef)
      BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/")
        .serve
        .compile.drain.as(ExitCode.Success)

    }

  }

}
