package com.bones.http4s

import cats.data.EitherT
import cats.effect.Sync
import cats.syntax.all._
import com.bones.http4s.BaseCrudInterpreter.contentType
import com.bones.httpcommon.ClassicCrudDef
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import org.http4s.dsl.Http4sDsl
import org.http4s._

object ClassicCrud {

  def get[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE, String],
    getFunc: ID => F[Either[E, A]])(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    implicit val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam if classicCrudDef.path == path =>
        classicCrudDef
          .pathStringToId(idParam)
          .leftMap(e => {
            val (ct, encoderFunc) =
              classicCrudDef.httpPathError.encoderForOptionalContent(contentType(req))
            BadRequest(encoderFunc(e), Header("Content-Type", ct))
          })
          .map(id => {
            getFunc(id)
              .flatMap({
                case Left(re) =>
                  val (ct, encoderFunc) =
                    classicCrudDef.httpData.errorEncoderForOptionalContent(contentType(req))
                  BadRequest(
                    encoderFunc(re),
                    Header("Content-Type", ct)
                  )(F, entityEncoder)
                case Right(ro) =>
                  val (ct, encoderFunc) =
                    classicCrudDef.httpData.encoderForOptionalContent(contentType(req))
                  Ok(
                    encoderFunc(ro),
                    Header("Content-Type", ct)
                  )
              })
          })
          .merge
    }
  }

  def put[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE, String],
    updateFunc: (ID, A) => F[Either[E, ID]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._

    HttpRoutes.of[F] {
      case req @ Method.PUT -> Root / path / idParam if classicCrudDef.path == path =>
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          id <- EitherT.fromEither[F] {
            classicCrudDef
              .pathStringToId(idParam)
              .leftMap(e => {
                val (ct, f) =
                  classicCrudDef.httpPathError.encoderForOptionalContent(contentType(req))
                H.BadRequest(f(e), Header("Content-Type", ct))
              })
          }
          in <- EitherT.fromEither[F] {
            classicCrudDef.httpData
              .validatorForContent(contentType(req))
              .toRight(UnsupportedMediaType())
              .flatMap {
                case (ct, f) => {
                  f(body, classicCrudDef.interpreterConfig.charset).left
                    .map(errs => {
                      val (outContentType, errorF) =
                        classicCrudDef.httpData.extractionErrorEncoderForContent(ct)
                      BadRequest(
                        errorF(ErrorResponse(errs.toList)),
                        Header("Content-Type", outContentType))
                    })
                    .map(a => (ct, a))
                }
              }
          }
          out <- EitherT[F, F[Response[F]], ID] {
            updateFunc
              .apply(id, in._2)
              .map(_.left.map(ce => {
                val (ct, f) = classicCrudDef.httpData.errorEncoderForContent(in._1)
                InternalServerError(f(ce), Header("Content-Type", ct))
              }))
          }
        } yield {
          val (ct, f) = classicCrudDef.httpId.encoderForContent(in._1)
          Ok(
            f(out),
            Header("Content-Type", ct)
          )
        }
        result.value.flatMap(_.merge)
    }
  }

  def delete[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, String, E, PE, String],
    deleteFunc: ID => F[Either[E, ID]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.DELETE -> Root / path / idParam if classicCrudDef.path == path =>
        val result = for {
          id <- EitherT.fromEither[F] {
            classicCrudDef
              .pathStringToId(idParam)
              .leftMap(e => {
                val (ct, f) =
                  classicCrudDef.httpPathError.encoderForOptionalContent(contentType(req))
                H.BadRequest(f(e), Header("Content-Type", ct))
              })
          }
          entityId <- EitherT[F, F[Response[F]], ID] {
            deleteFunc(id).map(
              _.left.map(err => {
                val (ct, encoder) =
                  classicCrudDef.httpData.errorEncoderForOptionalContent(contentType(req))
                InternalServerError(encoder(err), Header("Content-Type", ct))
              })
            )
          }
        } yield {
          val (ct, encoder) = classicCrudDef.httpId.encoderForOptionalContent(contentType(req))
          Ok(
            encoder(entityId),
            Header("Content-Type", ct)
          )
        }
        result.value.flatMap(_.merge)
    }
  }

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
