package com.bones.http4s

import cats.data.{EitherT, NonEmptyList}
import cats.effect.Sync
import com.bones.http4s.CrudInterpreterDescription.PutPostInterpreterGroup
import org.http4s.{Header, HttpRoutes, Method, Response}
import cats.effect._
import com.bones.data.Error.ExtractionError
import io.circe.Json
import org.http4s._
import org.http4s.dsl.Http4sDsl
//import org.http4s.dsl.io._
import org.http4s.util.CaseInsensitiveString

import scala.concurrent.ExecutionContext.Implicits.global

object Headers {
  /** Get content type from the Request Headers if it exists */
  def contentType[F[_]](req: Request[F]): Option[String] =
    req.headers
      .find(header => header.name == CaseInsensitiveString("Content-Type"))
      .map(_.value)


}

object ErrorResponse {

  def errorToOut[F[_], E](ee: NonEmptyList[E], eSchema: List[E] => Array[Byte])(implicit F: Sync[F], H: Http4sDsl[F]): F[Response[F]] = {
    import H._
    BadRequest.apply(eSchema.apply(ee.toList))
  }

}


