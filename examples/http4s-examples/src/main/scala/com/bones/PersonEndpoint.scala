package com.bones

import _root_.doobie.hikari._
import _root_.doobie.implicits._
import _root_.doobie.util.ExecutionContexts
import _root_.doobie.util.transactor.Transactor
import _root_.io.swagger.v3.oas.models.OpenAPI
import _root_.io.swagger.v3.oas.models.info.Info
import cats.effect._
import cats.implicits._
import com.bones.crud.Algebra._
import com.bones.data.Value.KvpNil
import com.bones.doobie.Algebra.{JsonFormat}
import com.bones.doobie.Orm.Dao
import com.bones.doobie.{HttpInterpreter, WithId}
import com.bones.oas3.CrudOasInterpreter
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{LongValidation => iv, StringValidation => sv}
import org.http4s.server.blaze._
import fs2.Stream




object Definitions {

  case class Person(name: String, age: Long, gender: Option[String])


  object Person {
    implicit val dao: Dao.Aux[Person, Long] =
      Dao.derive[Person, Long]("person", "id")
  }

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

  case class WithTransaction(xa: Transactor[IO]) {

    val searchF: Stream[IO, WithId[Person]] =
      Person.dao.findAll.transact(xa).map(i => WithId(i._1,i._2))

    val createF: Person => IO[Either[Error,WithId[Person]]] =
      person => Person.dao.insert(person).transact(xa)
        .map(id => Right(WithId(id,person)))

    val readF: Person.dao.Key => IO[Either[Error,WithId[Person]]] =
      (id) => Person.dao.find(id).transact(xa)
        .map(_.toRight(Error(s"Person with id ${id} not found")))
        .map(_.map(person => WithId[Person](id, person)))

    val updateF: (Long, Person) => IO[Either[Error, WithId[Person]]] =
      (id, person) => Person.dao.update(id, person).transact(xa)
        .map(i => Right(WithId(i,person)))

    val deleteF: Long => IO[Either[Error, WithId[Person]]] =
      (id) => for {
        person <- readF(id)
        _ <- Person.dao.delete(id).transact(xa)
      } yield person
  }
}




object PersonDoc extends App {
  val info = new Info()
    .description("Test Person Endpoint")
    .title("Person")
    .version("1.0")

  val openApi = new OpenAPI()

  CrudOasInterpreter.jsonApiForService(Definitions.personService).apply(openApi)


  println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi))
}

object PersonEndpoint extends IOApp {
  import Definitions._

  val transactor: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
  )


  val hikariTransactor: Resource[IO, HikariTransactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
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



  override def run(args: List[String]): IO[ExitCode] = {

    val service =
      HttpInterpreter("/person")
        .withContentType(JsonFormat)
        .withSwagger()

    hikariTransactor.use { xa =>
      val middleware = Definitions.WithTransaction(xa)
      val http4Service = service.forService(
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

}
