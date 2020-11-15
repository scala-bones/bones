package com.bones.http4s

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._
import com.bones.http4s.BaseCrudInterpreter.contentType
import com.bones.httpcommon.ClassicCrudDef
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import org.http4s.dsl.Http4sDsl
import org.http4s.{Header, HttpRoutes, Method, Response}

object ClassicCrud {

  def post[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE, String],
    createF: A => F[Either[E, ID]])(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {

    import H._
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path if classicCrudDef.path == path =>
        val result = classicCrudDef.httpData
          .validatorForContent(contentType(req))
          .map {
            case (contentType, validatorFunc) => {
              val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
                body <- EitherT[F, F[Response[F]], Array[Byte]] {
                  req.as[Array[Byte]].map(Right(_))
                }
                in <- EitherT.fromEither[F] {
                  validatorFunc(body, classicCrudDef.interpreterConfig.charset).left
                    .map(errs => {
                      val (outContentType, errorF) =
                        classicCrudDef.httpData.extractionErrorEncoderForContent(contentType)
                      BadRequest(
                        errorF(ErrorResponse(errs.toList)),
                        Header("Content-Type", outContentType))
                    })

                }
                out <- EitherT[F, F[Response[F]], ID] {
                  createF(in)
                    .map(_.left.map(ce => {
                      val (outContentType, errorF) =
                        classicCrudDef.httpData.errorEncoderForContent(contentType)

                      val out = errorF(ce)
                      InternalServerError(out, Header("Content-Type", outContentType))
                    }))
                }
              } yield {
                val (outContent, responseDataF) =
                  classicCrudDef.httpId.encoderForContent(contentType)
                Ok(
                  responseDataF(out),
                  Header("Content-Type", outContent)
                )
              }
              result.value.flatMap(_.merge)
            }
          }
        result.getOrElse(UnsupportedMediaType())
    }
  }

}
