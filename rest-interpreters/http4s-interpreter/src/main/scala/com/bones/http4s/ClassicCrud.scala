package com.bones.http4s

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._
import com.bones.http.common.ClassicCrudDef
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`

object ClassicCrud {

  /**
    * Creates a route as a classic GET request, where the route is /entity/{id}.
    * Parses the id and passes the value to method supplied by getFunc.
    * @return Route for use in HTTP4s
    */
  def get[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, `Content-Type`, E, PE],
    getFunc: ID => F[Either[E, (ID, A)]])(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    implicit val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam if classicCrudDef.path == path =>
        pathIdOrError(idParam, req.contentType, classicCrudDef)
          .map(pathId => {
            getFunc(pathId)
              .flatMap({
                case Left(re) => // getFunc returned the error state
                  val (contentType, encoder) =
                    classicCrudDef.errorResponseSchemaEncoders.encoderForOptionalContent(
                      req.contentType)
                  BadRequest(encoder.encode(re), contentType)
                case Right(ro) => // getFunc returned successfully
                  val (contentType, encoder) =
                    classicCrudDef.responseSchemaWithIdEncoder.encoderForOptionalContent(
                      req.contentType)
                  Ok(encoder.encode(ro), contentType)
              })
          })
          .merge
    }
  }

  def put[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, `Content-Type`, E, PE],
    updateFunc: (ID, A) => F[Either[E, ID]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._

    HttpRoutes.of[F] {
      case req @ Method.PUT -> Root / path / idParam if classicCrudDef.path == path =>
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          pathId <- EitherT.fromEither[F] {
            pathIdOrError(idParam, req.contentType, classicCrudDef)
          }
          in <- EitherT.fromEither[F] {
            classicCrudDef.requestSchemaValidators
              .validatorForOptionalContent(req.contentType)
              .toRight(UnsupportedMediaType(s"Unsupported Content-Type: '${req.contentType}''"))
              .flatMap {
                case (ct, f) =>
                  f.validate(body)
                    .left
                    .map(errs => {
                      val (outContentType, errorF) =
                        classicCrudDef.errorResponseEncoders
                          .encoderForContent(ct)
                      BadRequest(errorF.encode(ErrorResponse(errs)), outContentType)
                    })
                    .map(a => (ct, a))
              }
          }
          out <- EitherT[F, F[Response[F]], ID] {
            updateFunc(pathId, in._2)
              .map(_.left.map(ce => {
                val (conentType, f) =
                  classicCrudDef.errorResponseSchemaEncoders.encoderForContent(in._1)
                InternalServerError(f.encode(ce), conentType)
              }))
          }
        } yield {
          val (contentType, f) = classicCrudDef.idEncoders.encoderForContent(in._1)
          Ok(f.encode(out), contentType)
        }
        result.value.flatMap(_.merge)
    }
  }

  private def pathIdOrError[ALG[_], A, ID, E, PE, F[_]](
    idParam: String,
    contentType: Option[`Content-Type`],
    classicCrudDef: ClassicCrudDef[ALG, A, ID, `Content-Type`, E, PE])(
    implicit F: Sync[F],
    H: Http4sDsl[F]): Either[F[Response[F]], ID] = {
    import H._
    classicCrudDef
      .pathStringToId(idParam)
      .leftMap(e => {
        val (ct, f) =
          classicCrudDef.pathErrorEncoder.encoderForOptionalContent(contentType)
        H.BadRequest(f.encode(e), ct)
      })
  }

  def delete[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, `Content-Type`, E, PE],
    deleteFunc: ID => F[Either[E, ID]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.DELETE -> Root / path / idParam if classicCrudDef.path == path =>
        val result = for {
          pathId <- EitherT.fromEither[F] {
            pathIdOrError(idParam, req.contentType, classicCrudDef)
          }
          entityId <- EitherT[F, F[Response[F]], ID] {
            deleteFunc(pathId).map(
              _.left.map(err => {
                val (conentType, encoder) =
                  classicCrudDef.errorResponseSchemaEncoders.encoderForOptionalContent(
                    req.contentType)
                InternalServerError(encoder.encode(err), conentType)
              })
            )
          }
        } yield {
          val (contentType, encoder) =
            classicCrudDef.idEncoders.encoderForOptionalContent(req.contentType)
          Ok(
            encoder.encode(entityId),
            contentType
          )
        }
        result.value.flatMap(_.merge)
    }
  }

  def post[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, `Content-Type`, E, PE],
    createF: A => F[Either[E, ID]])(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {

    import H._
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path if classicCrudDef.path == path =>
        val result = classicCrudDef.requestSchemaValidators
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
                    .map(errs => {
                      val (outContentType, errorF) =
                        classicCrudDef.errorResponseEncoders.encoderForContent(contentType)
                      BadRequest(errorF.encode(ErrorResponse(errs.toList)), outContentType)
                    })

                }
                out <- EitherT[F, F[Response[F]], ID] {
                  createF(in)
                    .map(_.left.map(ce => {
                      val (outContentType, errorF) =
                        classicCrudDef.errorResponseSchemaEncoders.encoderForContent(contentType)

                      val out = errorF.encode(ce)
                      InternalServerError(out, outContentType)
                    }))
                }
              } yield {
                val (outContent, responseDataF) =
                  classicCrudDef.idEncoders.encoderForContent(contentType)
                Ok(
                  responseDataF.encode(out),
                  outContent
                )
              }
              result.value.flatMap(_.merge)
            }
          }
        result.getOrElse(UnsupportedMediaType())
    }
  }

}
