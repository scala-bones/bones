package com.bones.fullstack

import _root_.doobie.hikari._
import _root_.doobie.implicits._
import _root_.doobie.util.ExecutionContexts
import _root_.doobie.util.transactor.Transactor
import _root_.io.swagger.v3.oas.models.OpenAPI
import _root_.io.swagger.v3.oas.models.info.Info
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.bones.Definitions
import com.bones.doobie.Algebra.JsonFormat
import com.bones.doobie.HttpInterpreter
import doobie.hikari.HikariTransactor
import doobie.util.ExecutionContexts
import org.http4s.server.blaze.BlazeBuilder

abstract class LocalhostAllIOApp[A](postgresSchema: String) extends IOApp {

  import Definitions._

  val transactor: Transactor.Aux[IO, Unit] = Transactor.fromDriverManager[IO](
    s"org.postgresql.Driver", s"jdbc:postgresql:${postgresSchema}", "postgresSchema", ""
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

//      BlazeBuilder[IO].bindHttp(8080, "localhost").mountService(http4Service, "/")
//        .serve
//        .compile.drain.as(ExitCode.Success)

      ???

    }
  }


}



