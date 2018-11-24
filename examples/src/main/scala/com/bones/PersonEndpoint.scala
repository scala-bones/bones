package com.bones

import cats.effect.{IO, IOApp}
import com.bones.Definitions.personWithId
import com.bones.crud.Algebra._
import com.bones.data.Value.{DataClass, KvpNil}
import com.bones.http4s.Algebra.jsonFormat
import com.bones.http4s.{HttpInterpreter, Orm}
import com.bones.http4s.Orm.Dao
import com.bones.oas3.CrudOasInterpreter
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{LongValidation => iv, StringValidation => sv}
import doobie.Transactor
import doobie.hikari.HikariTransactor
import doobie.util.transactor.Transactor.Aux
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.info.Info
import org.http4s.HttpService


object Definitions {

  case class Person(name: String, age: Long, gender: Option[String])
  case class WithId[P](id: Long, p: P)

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
    (kvp("id", long) :: personSchema :: KvpNil).convert[WithId[Person]]

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
  val ps = Definitions.personService

  ps.createOperation.foreach(co => CrudOasInterpreter.post(
    (co.schemaForCreate, ps.path),
    (co.successSchemaForCreate, ps.path),
    (co.errorSchemaForCreate, "Error"),
    s"/${ps.path}",
    List("application/json")
  ).apply(openApi))

  ps.readOperation.foreach(read =>
    CrudOasInterpreter.get( (read.successSchemaForRead, ps.path), s"/${ps.path}").apply(openApi)
  )

  ps.updateOperation.foreach(update =>
    CrudOasInterpreter.put(
      (update.inputSchema, ps.path),
      (update.successSchema, ps.path),
      (update.failureSchema, "Error"),
      s"/${ps.path}",
      List("application/json")
    ).apply(openApi)
  )

  ps.deleteOperation.foreach(delete =>
    CrudOasInterpreter
      .delete( (delete.successSchema, ps.path), s"/${ps.path}", List("application/json") )
      .apply(openApi)
  )

  println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi))
}

object PersonEndpoint extends IOApp {
  import Definitions._

  val transactor: Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver", "jdbc:postgresql:bones", "postgres", ""
  )

  import cats.effect._
  import cats.implicits._
  import doobie._
  import doobie.hikari._
  import org.http4s.server.blaze._

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

  override def run(args: List[String]): IO[ExitCode] = {

    val service =
      HttpInterpreter("/person")
        .withContentType(jsonFormat)
        .withSwagger()

    val createF: Person => IO[Either[Error,WithId[Person]]] =
      (person) => {
        Orm.withHikari[Person.dao.Key](Person.dao.insert(person),hikariTransactor)
          .map(id => Right(WithId(id,person)))
      }
    val readF: Person.dao.Key => IO[Either[Error,WithId[Person]]] =
      (id) => {
        Orm.withHikari[Option[Person]](Person.dao.find(id), hikariTransactor)
          .map(_.toRight(Error(s"Person with id ${id} not found")))
          .map(_.map(person => WithId[Person](id, person)))
      }
    val updateF: (Long, Person) => IO[Either[Error, WithId[Person]]] =
      (id, person) =>
        Orm.withHikari(Person.dao.update(id, person), hikariTransactor)
        .map(i => Right(WithId(i,person)))

    val deleteF: Long => IO[Either[Error, WithId[Person]]] =
      (id) => for {
        person <- readF(id)
        _ <- Orm.withHikari(Person.dao.delete(id), hikariTransactor)
      } yield person


    val http4Service = service.forService(
      personService,
      createF,
      readF,
      updateF,
      deleteF
    )

    BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/")
      .serve
      .compile.drain.as(ExitCode.Success)



  }

}
