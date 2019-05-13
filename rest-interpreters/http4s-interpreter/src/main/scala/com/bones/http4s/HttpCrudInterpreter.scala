package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.Functor
import cats.data.Kleisli
import cats.effect.IO
import com.bones.circe.EncodeToCirceInterpreter
import com.bones.crud.Algebra.{CrudLayer, Read}
import com.bones.http4s.HttpInterpreter.GetInterpreterGroup
import org.http4s.HttpRoutes

case class HttpCrudInterpreter() {

  type ID = Long

  val charset: java.nio.charset.Charset = StandardCharsets.UTF_8

  type Path = String

  def jsonEndpoint[I,O](op: CrudLayer[I,O]): (I => IO[O], Path) => HttpRoutes[IO] = {
    op match {
      case get: Read[o,e] =>
        val outputF =
          EncodeToCirceInterpreter.fromSchema(get.outputSchema)
        val errorF = EncodeToCirceInterpreter.fromSchema(get.errorSchema)
        val json = GetInterpreterGroup[o, e](
          "application/json",
          ro => outputF(ro).spaces2.getBytes(charset),
          re => errorF(re).spaces2.getBytes(charset)
        )
        (a,path) => {
          ??? // HttpInterpreter.get(path, json, a)
        }
    }
  }

}
