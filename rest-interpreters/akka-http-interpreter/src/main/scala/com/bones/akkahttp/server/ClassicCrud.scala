package com.bones.akkahttp.server

import akka.http.scaladsl.model.StatusCodes.{InternalServerError, UnsupportedMediaType}
import akka.http.scaladsl.model.headers.`Content-Type`
import akka.http.scaladsl.model.{ContentType, StatusCodes}
import akka.http.scaladsl.server.directives.{
  BasicDirectives,
  FutureDirectives,
  MarshallingDirectives,
  MethodDirectives,
  PathDirectives,
  RouteDirectives
}
import akka.http.scaladsl.server.{Directives, Route}
import com.bones.http.common.ClassicCrudDef
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse

import scala.concurrent.Future
import scala.util.{Failure, Success}

object ClassicCrud
    extends PathDirectives
    with MethodDirectives
    with BasicDirectives
    with MarshallingDirectives
    with RouteDirectives
    with FutureDirectives {

  def endpointForCreate[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, ContentType, E, PE],
    f: A => Future[Either[E, (ID, A)]]
  ): Route = ignoreTrailingSlash {
    path(classicCrudDef.path) {
      post {
        extractRequest { request =>
          entity(as[Array[Byte]]) { bytes =>
            {
              val contentType = request.entity.contentType
              classicCrudDef.requestSchemaValidators
                .validatorForContent(contentType)
                .toRight(complete(UnsupportedMediaType))
                .map {
                  case (ct, validator) => {
                    validator.validate(bytes) match {
                      case Left(err) =>
                        val encoder =
                          classicCrudDef.errorResponseEncoders.encoderForContent(ct)
                        complete(
                          StatusCodes.UnprocessableEntity,
                          List(`Content-Type`(encoder._1)),
                          encoder._2.encode(ErrorResponse(err.toList))
                        )
                      case Right(value) =>
                        onComplete(f(value)) {
                          case Success(result) =>
                            result match {
                              case Left(systemError) =>
                                val encoder =
                                  classicCrudDef.errorResponseSchemaEncoders.encoderForContent(ct)
                                complete(
                                  StatusCodes.InternalServerError,
                                  List(`Content-Type`(encoder._1)),
                                  encoder._2.encode(systemError)
                                )
                              case Right(value) =>
                                val encoder =
                                  classicCrudDef.responseSchemaWithIdEncoder.encoderForContent(ct)
                                complete(
                                  StatusCodes.OK,
                                  List(`Content-Type`(encoder._1)),
                                  encoder._2.encode(value)
                                )
                            }
                          case Failure(ex) =>
                            complete(InternalServerError, s"An error occurred: ${ex.getMessage}")
                        }
                    }
                  }
                }
                .merge
            }
          }
        }
      }
    }
  }

  def getEndpoint[ALG[_], A, ID, E, PE, F[_]](
    classicCrudDef: ClassicCrudDef[ALG, A, ID, ContentType, E, PE],
    getFunc: ID => Future[Either[E, (ID, A)]]
  ): Route = ignoreTrailingSlash {
    pathPrefix(classicCrudDef.path / Segment) { idString =>
      get {
        extractRequest { request =>
          classicCrudDef.pathStringToId(idString) match {
            case Left(err) =>
              val encoder =
                classicCrudDef.pathErrorEncoder.encoderForContent(request.entity.contentType)
              complete(
                StatusCodes.UnprocessableEntity,
                List(`Content-Type`(encoder._1)),
                encoder._2.encode(err)
              )
            case Right(id) => {
              onComplete(getFunc(id)) {
                case Success(value) => {
                  value match {
                    case Left(systemError) =>
                      val encoder = classicCrudDef.errorResponseSchemaEncoders.encoderForContent(
                        request.entity.contentType
                      )
                      complete(
                        StatusCodes.InternalServerError,
                        List(`Content-Type`(encoder._1)),
                        encoder._2.encode(systemError)
                      )
                    case Right(value) =>
                      val encoder = classicCrudDef.responseSchemaWithIdEncoder.encoderForContent(
                        request.entity.contentType
                      )
                      complete(
                        StatusCodes.OK,
                        List(`Content-Type`(encoder._1)),
                        encoder._2.encode(value)
                      )
                  }
                }
                case Failure(ex) =>
                  complete(InternalServerError, s"An error occurred: ${ex.getMessage}")
              }
            }
          }
        }
      }
    }
  }

}
