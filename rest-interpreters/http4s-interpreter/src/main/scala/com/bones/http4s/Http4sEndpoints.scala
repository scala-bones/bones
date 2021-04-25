package com.bones.http4s

import cats.data.EitherT
import cats.effect.{Concurrent, Sync}
import com.bones.http.common.{HttpEndpointDef, StringToIdError}
import org.http4s.{Header, HttpRoutes, Method, Response}
import org.http4s.dsl.Http4sDsl
import org.http4s._
import cats.syntax.all._
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import org.http4s.headers.`Content-Type`

object Http4sEndpoints {

  /**
    * Create a get endpoint.
    */
  def get[F[_], ALG[_], RES, ID, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, _, RES, `Content-Type`, E, PE],
    stringParamToId: String => Either[PE, ID],
    readF: ID => F[Either[E, RES]]
  )(implicit F: Concurrent[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    implicit val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam if expectedPath == path =>
        val (contentType, encoder) =
          endpointDef.responseSchemaEncoders.encoderForOptionalContent(req.contentType)
        stringParamToId(idParam)
          .leftMap(e => {
            val (errContentType, errorEncoder) =
              endpointDef.pathErrorEncoder.encoderForContent(contentType)
            BadRequest(errorEncoder.encode(e), errContentType)
          })
          .map(id => {
            readF(id)
              .flatMap({
                case Left(re) =>
                  val (errContentType, errEncoder) =
                    endpointDef.errorResponseSchemaEncoders.encoderForContent(contentType)
                  BadRequest(
                    errEncoder.encode(re),
                    errContentType
                  )
                case Right(ro) =>
                  Ok(
                    encoder.encode(ro),
                    contentType
                  )
              })
          })
          .merge
    }
  }

  /**
    * Create a PUT endpoint given serialization functions and business logic.
    * The expected path is /Root/$expectedPath/{id}
    *
    * @param endpointDef contains functions to and from Array[Byte]
    * @param updateF          Business logic to execute after
    * @return
    */
  def put[F[_], ALG[_], REQ, RES, ID, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, REQ, RES, `Content-Type`, E, PE],
    stringParamToId: String => Either[PE, ID],
    updateF: (ID, REQ) => F[Either[E, RES]]
  )(implicit F: Concurrent[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._

    HttpRoutes.of[F] {
      case req @ Method.PUT -> Root / path / idParam if expectedPath == path =>
        endpointDef.requestSchemaValidators
          .validatorForOptionalContent(req.contentType)
          .map {
            case (ct, validator) => {

              val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
                body <- EitherT[F, F[Response[F]], Array[Byte]] {
                  req.as[Array[Byte]].map(Right(_))
                }
                id <- EitherT.fromEither[F] {
                  stringParamToId(idParam).leftMap(e => {
                    val (errContentType, errEncoder) =
                      endpointDef.pathErrorEncoder.encoderForContent(ct)
                    BadRequest(
                      errEncoder.encode(e),
                      errContentType
                    )
                  })
                }
                in <- EitherT.fromEither[F] {
                  validator
                    .validate(body)
                    .left
                    .map(err => {
                      val (errContentType, errEncoder) =
                        endpointDef.errorResponseEncoders.encoderForContent(ct)
                      BadRequest(errEncoder.encode(ErrorResponse(err)), errContentType)
                    })
                }
                out <- EitherT[F, F[Response[F]], RES] {
                  updateF(id, in)
                    .map(_.left.map(ce => {
                      val (errContentType, errEncoder) =
                        endpointDef.errorResponseSchemaEncoders.encoderForContent(ct)
                      InternalServerError(errEncoder.encode(ce), errContentType)
                    }))
                }
              } yield {
                val (resContentType, resEncoder) =
                  endpointDef.responseSchemaEncoders.encoderForContent(ct)
                Ok(
                  resEncoder.encode(out),
                  resContentType
                )
              }
              result.value.flatMap(_.merge)
            }
          }
          .getOrElse(UnsupportedMediaType())

    }
  }

  /** Create the post endpoint from the Interpreter Group */
  def post[F[_], ALG[_], REQ, RES, ID, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, REQ, RES, `Content-Type`, E, PE],
    createF: REQ => F[Either[E, RES]]
  )(implicit F: Concurrent[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path if expectedPath == path =>
        endpointDef.requestSchemaValidators
          .validatorForOptionalContent(req.contentType)
          .map {
            case (contentType, validator) => {

              val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
                body <- EitherT[F, F[Response[F]], Array[Byte]] {
                  req.as[Array[Byte]].map(Right(_))
                }
                in <- EitherT.fromEither[F] {
                  validator
                    .validate(body)
                    .left
                    .map(x => {
                      val (errContentType, errEncoder) =
                        endpointDef.errorResponseEncoders.encoderForContent(contentType)
                      BadRequest(errEncoder.encode(ErrorResponse(x)), errContentType)
                    })
                }
                out <- EitherT[F, F[Response[F]], RES] {
                  createF(in)
                    .map(_.left.map(ce => {
                      val (errContentType, errEncoder) =
                        endpointDef.errorResponseSchemaEncoders.encoderForContent(contentType)
                      InternalServerError(errEncoder.encode(ce), errContentType)
                    }))
                }
              } yield {
                val (resContentType, resEncoder) =
                  endpointDef.responseSchemaEncoders.encoderForContent(contentType)
                Ok(
                  resEncoder.encode(out),
                  resContentType
                )
              }
              result.value.flatMap(_.merge)
            }
          }
          .getOrElse(UnsupportedMediaType())

    }
  }

  /** Create delete routes from interpreter group */
  def delete[F[_], ALG[_], RES, ID, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, _, RES, `Content-Type`, E, PE],
    stringParamToId: String => Either[PE, ID],
    deleteF: ID => F[Either[E, RES]]
  )(implicit F: Concurrent[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.DELETE -> Root / path / idParam if (expectedPath == path) =>
        val reqContentType = req.contentType
        stringParamToId(idParam)
          .leftMap(err => {
            val (errContentType, errEncoder) =
              endpointDef.pathErrorEncoder.encoderForOptionalContent(reqContentType)
            BadRequest(errEncoder.encode(err), errContentType)
          })
          .map(id => {
            deleteF(id).flatMap {
              case Right(entity) =>
                val (resContentType, resEncoder) =
                  endpointDef.responseSchemaEncoders.encoderForOptionalContent(reqContentType)
                Ok(
                  resEncoder.encode(entity),
                  resContentType
                )
              case Left(err) =>
                val (errContentType, errEncoder) =
                  endpointDef.errorResponseSchemaEncoders.encoderForOptionalContent(reqContentType)
                InternalServerError(errEncoder.encode(err), errContentType)
            }
          })
          .merge
    }
  }

  /** Create search endpoints form the Interpreter Group */
  def search[F[_], ALG[_], RES, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, _, RES, `Content-Type`, E, PE],
    searchF: () => fs2.Stream[F, RES]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path if path == expectedPath =>
        val (contentType, encoder) =
          endpointDef.responseSchemaEncoders.encoderForOptionalContent(req.contentType)
        Ok(
          searchF().map(res => encoder.encode(res)),
          contentType
        )
    }
  }

}
