package com.bones.http4s

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._
import com.bones.http.common.ClassicCrudDef
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import org.http4s.dsl.Http4sDsl
import org.http4s._

object ClassicCrud {

  /**
    * Creates a route as a classic GET request, where the route is /entity/{id}.
    * Parses the id and passes the value to method supplied by getFunc.
    * @return Route for use in HTTP4s
    */
  def get[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE],
    getFunc: ID => F[Either[E, (ID, A)]])(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    implicit val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam if classicCrudDef.path == path =>
        pathIdOrError(idParam, findContentType(req), classicCrudDef)
          .map(pathId => {
            getFunc(pathId)
              .flatMap({
                case Left(re) => // getFunc returned the error state
                  val (ct, encoderFunc) =
                    classicCrudDef.errorResponseSchemaEncoders.encoderForOptionalContent(
                      findContentType(req))
                  BadRequest(encoderFunc(re), Header("Content-Type", ct))
                case Right(ro) => // getFunc returned successfully
                  val (ct, encoderFunc) =
                    classicCrudDef.responseSchemaWithIdEncoder.encoderForOptionalContent(
                      findContentType(req))
                  Ok(encoderFunc(ro), Header("Content-Type", ct))
              })
          })
          .merge
    }
  }

  def put[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE],
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
            pathIdOrError(idParam, findContentType(req), classicCrudDef)
          }
          in <- EitherT.fromEither[F] {
            classicCrudDef.requestSchemaValidators
              .validatorForOptionalContent(findContentType(req))
              .toRight(
                UnsupportedMediaType(s"Unsupported Content-Type: '${findContentType(req)}''"))
              .flatMap {
                case (ct, f) =>
                  f(body).left
                    .map(errs => {
                      val (outContentType, errorF) =
                        classicCrudDef.errorResponseEncoders
                          .encoderForContent(ct)
                      BadRequest(
                        errorF(ErrorResponse(errs)),
                        Header("Content-Type", outContentType))
                    })
                    .map(a => (ct, a))
              }
          }
          out <- EitherT[F, F[Response[F]], ID] {
            updateFunc(pathId, in._2)
              .map(_.left.map(ce => {
                val (ct, f) =
                  classicCrudDef.errorResponseSchemaEncoders.encoderForContent(in._1)
                InternalServerError(f(ce), Header("Content-Type", ct))
              }))
          }
        } yield {
          val (ct, f) = classicCrudDef.idEncoders.encoderForContent(in._1)
          Ok(f(out), Header("Content-Type", ct))
        }
        result.value.flatMap(_.merge)
    }
  }

  private def pathIdOrError[ALG[_], A, ID, E, PE, F[_]](
    idParam: String,
    contentType: Option[String],
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE])(
    implicit F: Sync[F],
    H: Http4sDsl[F]): Either[F[Response[F]], ID] = {
    import H._
    classicCrudDef
      .pathStringToId(idParam)
      .leftMap(e => {
        val (ct, f) =
          classicCrudDef.pathErrorEncoder.encoderForOptionalContent(contentType)
        H.BadRequest(f(e), Header("Content-Type", ct))
      })
  }

  def delete[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE],
    deleteFunc: ID => F[Either[E, ID]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.DELETE -> Root / path / idParam if classicCrudDef.path == path =>
        val result = for {
          pathId <- EitherT.fromEither[F] {
            pathIdOrError(idParam, findContentType(req), classicCrudDef)
          }
          entityId <- EitherT[F, F[Response[F]], ID] {
            deleteFunc(pathId).map(
              _.left.map(err => {
                val (ct, encoder) =
                  classicCrudDef.errorResponseSchemaEncoders.encoderForOptionalContent(
                    findContentType(req))
                InternalServerError(encoder(err), Header("Content-Type", ct))
              })
            )
          }
        } yield {
          val (ct, encoder) =
            classicCrudDef.idEncoders.encoderForOptionalContent(findContentType(req))
          Ok(
            encoder(entityId),
            Header("Content-Type", ct)
          )
        }
        result.value.flatMap(_.merge)
    }
  }

  def post[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE],
    createF: A => F[Either[E, ID]])(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {

    import H._
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path if classicCrudDef.path == path =>
        val result = classicCrudDef.requestSchemaValidators
          .validatorForOptionalContent(findContentType(req))
          .map {
            case (contentType, validatorFunc) => {
              val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
                body <- EitherT[F, F[Response[F]], Array[Byte]] {
                  req.as[Array[Byte]].map(Right(_))
                }
                in <- EitherT.fromEither[F] {
                  validatorFunc(body).left
                    .map(errs => {
                      val (outContentType, errorF) =
                        classicCrudDef.errorResponseEncoders.encoderForContent(contentType)
                      BadRequest(
                        errorF(ErrorResponse(errs.toList)),
                        Header("Content-Type", outContentType))
                    })

                }
                out <- EitherT[F, F[Response[F]], ID] {
                  createF(in)
                    .map(_.left.map(ce => {
                      val (outContentType, errorF) =
                        classicCrudDef.errorResponseSchemaEncoders.encoderForContent(contentType)

                      val out = errorF(ce)
                      InternalServerError(out, Header("Content-Type", outContentType))
                    }))
                }
              } yield {
                val (outContent, responseDataF) =
                  classicCrudDef.idEncoders.encoderForContent(contentType)
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
