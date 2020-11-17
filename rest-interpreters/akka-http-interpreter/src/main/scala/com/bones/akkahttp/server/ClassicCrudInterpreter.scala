package com.bones.akkahttp.server

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{HttpHeader, StatusCodes}
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers.byteArrayUnmarshaller
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import spray.json.{JsObject, JsString}

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class ClassicCrudInterpreter[ALG[_], ID, A, E](
  definition: ClassicCrudDefinition[ALG, ID, A, E])
    extends Directives
    with SprayJsonSupport {

  def createRoutes(): List[Route] = {
    definition.createF.map(routeForCreate).toList :::
      definition.readF.map(routeForGet).toList
  }

  def routeForGet(f: ID => Future[Either[E, (ID, A)]]): Route = {
    pathPrefix(definition.urlPath / Remaining) { idString =>
      get {
        definition.pathStringToId(idString) match {
          case Left(err) =>
            complete(StatusCodes.UnprocessableEntity, JsObject(("errorMessage", JsString(err))))
          case Right(id) =>
            onComplete(f(id)) {
              case Success(value) =>
                value match {
                  case Left(systemError) =>
                    complete(
                      StatusCodes.InternalServerError,
                      definition.errorMarshallerFunction(systemError))
                  case Right(value) =>
                    complete(StatusCodes.OK, List.empty, definition.schemaMarshallerFunction(value))
                }
              case Failure(ex) =>
                complete(InternalServerError, s"An error occurred: ${ex.getMessage}")
            }
        }
      }
    }
  }

  /**
    * Defines a POST route at /${definition.urlPath} for creating a new A
    * @param f The function to execute if we are able to unmarshall the request into an A without validation errors.
    * @return The POST Route.
    */
  def routeForCreate(f: A => Future[Either[E, ID]]): Route = {
    path(definition.urlPath) {
      post {
        entity(as[Array[Byte]]) { bytes =>
          {
            definition.schemaUnmarshallerFunction(bytes) match {
              case Left(err) =>
                complete(
                  StatusCodes.UnprocessableEntity,
                  definition.validationErrorEncoder(ErrorResponse(err.toList)))
              case Right(value) =>
                onComplete(f(value)) {
                  case Success(value) =>
                    value match {
                      case Left(systemError) =>
                        complete(
                          StatusCodes.InternalServerError,
                          definition.errorMarshallerFunction(systemError))
                      case Right(value) =>
                        complete(
                          StatusCodes.OK,
                          List.empty,
                          definition.idSchemaMarshallerFunction(value))
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
