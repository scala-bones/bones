package com.bones.http4s

import cats.data.EitherT
import cats.effect.Sync
import com.bones.http.common.{HttpEndpointDef, StringToIdError}
import org.http4s.{Header, HttpRoutes, Method, Response}
import org.http4s.dsl.Http4sDsl
import org.http4s._
import cats.syntax.all._
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse

object Http4sEndpoints {

  /**
    * Create a get endpoint.
    */
  def get[F[_], ALG[_], RES, ID, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, _, RES, String, E, PE],
    stringParamToId: String => Either[PE, ID],
    readF: ID => F[Either[E, RES]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    implicit val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam if expectedPath == path =>
        val (contentType, encoder) =
          endpointDef.responseSchemaEncoders.encoderForOptionalContent(findContentType(req))
        stringParamToId(idParam)
          .leftMap(e => {
            val (errContentType, errorEncoder) =
              endpointDef.pathErrorEncoder.encoderForContent(contentType)
            BadRequest(errorEncoder(e), Header("Content-Type", errContentType))
          })
          .map(id => {
            readF(id)
              .flatMap({
                case Left(re) =>
                  val (errContentType, errEncoder) =
                    endpointDef.errorResponseSchemaEncoders.encoderForContent(contentType)
                  BadRequest(
                    errEncoder(re),
                    Header("Content-Type", errContentType)
                  )
                case Right(ro) =>
                  Ok(
                    encoder(ro),
                    Header("Content-Type", contentType)
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
    endpointDef: HttpEndpointDef[ALG, REQ, RES, String, E, PE],
    stringParamToId: String => Either[PE, ID],
    updateF: (ID, REQ) => F[Either[E, RES]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._

    HttpRoutes.of[F] {
      case req @ Method.PUT -> Root / path / idParam if expectedPath == path =>
        endpointDef.requestSchemaValidators
          .validatorForOptionalContent(findContentType(req))
          .map {
            case (ct, validatorFunc) => {

              val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
                body <- EitherT[F, F[Response[F]], Array[Byte]] {
                  req.as[Array[Byte]].map(Right(_))
                }
                id <- EitherT.fromEither[F] {
                  stringParamToId(idParam).leftMap(e => {
                    val (errCt, errEncoder) =
                      endpointDef.pathErrorEncoder.encoderForContent(ct)
                    BadRequest(
                      errEncoder(e),
                      Header("Content-Type", errCt)
                    )
                  })
                }
                in <- EitherT.fromEither[F] {
                  validatorFunc(body).left
                    .map(err => {
                      val (errCt, errEncoder) =
                        endpointDef.errorResponseEncoders.encoderForContent(ct)
                      BadRequest(errEncoder(ErrorResponse(err)), Header("Content-Type", errCt))
                    })
                }
                out <- EitherT[F, F[Response[F]], RES] {
                  updateF(id, in)
                    .map(_.left.map(ce => {
                      val (errCt, errEncoder) =
                        endpointDef.errorResponseSchemaEncoders.encoderForContent(ct)
                      InternalServerError(errEncoder(ce), Header("Content-Type", errCt))
                    }))
                }
              } yield {
                val (resContentType, resEncoder) =
                  endpointDef.responseSchemaEncoders.encoderForContent(ct)
                Ok(
                  resEncoder(out),
                  Header("Content-Type", resContentType)
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
    endpointDef: HttpEndpointDef[ALG, REQ, RES, String, E, PE],
    createF: REQ => F[Either[E, RES]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path if expectedPath == path =>
        endpointDef.requestSchemaValidators
          .validatorForOptionalContent(findContentType(req))
          .map {
            case (contentType, validator) => {

              val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
                body <- EitherT[F, F[Response[F]], Array[Byte]] {
                  req.as[Array[Byte]].map(Right(_))
                }
                in <- EitherT.fromEither[F] {
                  validator(body).left
                    .map(x => {
                      val (errContentType, errEncoder) =
                        endpointDef.errorResponseEncoders.encoderForContent(contentType)
                      BadRequest(
                        errEncoder(ErrorResponse(x)),
                        Header("Content-Type", errContentType))
                    })
                }
                out <- EitherT[F, F[Response[F]], RES] {
                  createF(in)
                    .map(_.left.map(ce => {
                      val (errContentType, errEncoder) =
                        endpointDef.errorResponseSchemaEncoders.encoderForContent(contentType)
                      InternalServerError(errEncoder(ce), Header("Content-Type", errContentType))
                    }))
                }
              } yield {
                val (resContentType, resEncoder) =
                  endpointDef.responseSchemaEncoders.encoderForContent(contentType)
                Ok(
                  resEncoder(out),
                  Header("Content-Type", resContentType)
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
    endpointDef: HttpEndpointDef[ALG, _, RES, String, E, PE],
    stringParamToId: String => Either[PE, ID],
    deleteF: ID => F[Either[E, RES]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.DELETE -> Root / path / idParam if (expectedPath == path) =>
        val reqContentType = findContentType(req)
        stringParamToId(idParam)
          .leftMap(err => {
            val (errContentType, errEncoder) =
              endpointDef.pathErrorEncoder.encoderForOptionalContent(reqContentType)
            BadRequest(errEncoder(err), Header("Content-Type", errContentType))
          })
          .map(id => {
            deleteF(id).flatMap {
              case Right(entity) =>
                val (resContentType, resEncoder) =
                  endpointDef.responseSchemaEncoders.encoderForOptionalContent(reqContentType)
                Ok(
                  resEncoder(entity),
                  Header("Content-Type", resContentType)
                )
              case Left(err) =>
                val (errContentType, errEncoder) =
                  endpointDef.errorResponseSchemaEncoders.encoderForOptionalContent(reqContentType)
                InternalServerError(errEncoder(err), Header("Content-Type", errContentType))
            }
          })
          .merge
    }
  }

  /** Create search endpoints form the Interpreter Group */
  def search[F[_], ALG[_], RES, E, PE](
    expectedPath: String,
    endpointDef: HttpEndpointDef[ALG, _, RES, String, E, PE],
    searchF: () => fs2.Stream[F, RES]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path if path == expectedPath =>
        val (contentType, encoder) =
          endpointDef.responseSchemaEncoders.encoderForOptionalContent(findContentType(req))
        Ok(
          searchF().map(res => encoder(res)),
          Header("Content-Type", contentType)
        )
    }
  }

}
