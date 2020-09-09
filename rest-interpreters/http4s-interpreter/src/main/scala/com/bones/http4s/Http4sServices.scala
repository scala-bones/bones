package com.bones.http4s

import cats.effect.{ExitCode, IO}
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

class Http4sServices {

  def createRoutes(
    services: HttpRoutes[IO],
    blazeServerBuilder: BlazeServerBuilder[IO]): IO[ExitCode] = {
    val router = Router("/api" -> services).orNotFound

    blazeServerBuilder
      .bindHttp(8080, "localhost")
      .withHttpApp(router)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }

}
