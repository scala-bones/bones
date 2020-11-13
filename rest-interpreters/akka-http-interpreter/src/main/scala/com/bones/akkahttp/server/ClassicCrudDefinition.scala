package com.bones.akkahttp.server

import java.nio.charset.Charset

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshalling.PredefinedToEntityMarshallers.byteArrayMarshaller
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model.{ContentType, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.AsyncAuthenticator
import akka.http.scaladsl.server.{Directives, Route, StandardRoute}
import akka.http.scaladsl.unmarshalling.PredefinedFromEntityUnmarshallers.byteArrayUnmarshaller
import com.bones.akkahttp.server.config.InterpreterConfig
import com.bones.data.values.ScalaCoreValue
import com.bones.data.{KvpCollection, KvpNil}
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.interpreter.values.ExtractionErrorEncoder.ErrorResponse
import com.bones.sprayJsValue.SprayValidatorInterpreter
import com.bones.sprayjson.sprayjson.IsoSprayEncoderInterpreter
import com.bones.sprayjson.values.BaseScalaCoreEncoder
import spray.json.{JsObject, JsString}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}

object ClassicCrudDefinition {
  def empty[ALG[_], ID: Manifest, A: Manifest, E](
    config: InterpreterConfig[ALG, ID],
    path: String,
    schema: KvpCollection[String, ALG, A],
    pathStringToId: String => Either[String, ID],
    errorSchema: KvpCollection[String, ALG, E])(
    implicit executionContext: ExecutionContext): ClassicCrudDefinition[ALG, ID, A, E] =
    ClassicCrudDefinition(config, path, schema, pathStringToId, errorSchema)

  def schemaUnmarshaller[ALG[_], A](
    schema: KvpCollection[String, ALG, A],
    jsonValidator: SprayValidatorInterpreter[ALG],
    charset: Charset) = {

    val f = jsonValidator.generateByteArrayValidator(schema, charset)

    byteArrayUnmarshaller.map(bytes => f.apply(bytes))
  }

  val isoSprayEncoderInterpreter =
    new IsoSprayEncoderInterpreter[ScalaCoreValue](
      BaseScalaCoreEncoder
    )

}

case class Authentication[T](realm: String, authenticator: AsyncAuthenticator[T])

case class ClassicCrudDefinition[ALG[_], ID: Manifest, A: Manifest, E](
  config: InterpreterConfig[ALG, ID],
  urlPath: String,
  schema: KvpCollection[String, ALG, A],
  pathStringToId: String => Either[String, ID],
  errorSchema: KvpCollection[String, ALG, E],
  createF: Option[A => Future[Either[E, ID]]] = None,
  readF: Option[ID => Future[Either[E, (ID, A)]]] = None,
  updateF: Option[(ID, A) => Future[Either[E, ID]]] = None,
  deleteF: Option[ID => Future[Either[E, (ID, A)]]] = None,
  searchF: Option[() => Future[Either[E, List[A]]]] = None)(
  implicit executionContext: ExecutionContext)
    extends Directives
    with SprayJsonSupport {

  val schemaWithId: KvpCollection[String, ALG, (ID, A)] = {
    (("id", config.idDefinition) :: schema :: new KvpNil[String, ALG]).tupled[(ID, A)]
  }

  val idSchema: KvpCollection[String, ALG, ID] =
    (("id", config.idDefinition) :: new KvpNil[String, ALG]).encodedHead()

  val validationErrorEncoder =
    ClassicCrudDefinition.isoSprayEncoderInterpreter.generateEncoder(
      ExtractionErrorEncoder.errorResponseSchema)

  val idSchemaMarshallerFunction =
    config.jsonEncoder.generateEncoder(idSchema)

  val schemaUnmarshallerFunction =
    config.jsonValidator.generateByteArrayValidator(schema, config.charset)
  val schemaUnmarshaller =
    byteArrayUnmarshaller.map(bytes => schemaUnmarshallerFunction.apply(bytes))

  val schemaMarshallerFunction =
    config.jsonEncoder.generateEncoder(schemaWithId)

  val schemaMarshaller = byteArrayMarshaller(ContentType.apply(MediaTypes.`application/json`))
    .compose[(ID, A)](output => {
      val json = schemaMarshallerFunction(output)
      json.compactPrint.getBytes(config.charset)
    })

  val errorMarshallerFunction = config.jsonEncoder.generateEncoder(errorSchema)

  val errorMarshaller = byteArrayMarshaller(ContentType.apply(MediaTypes.`application/json`))
    .compose[E](err => {
      errorMarshallerFunction(err).compactPrint.getBytes(config.charset)
    })

  def withCreate(f: A => Future[Either[E, ID]]): ClassicCrudDefinition[ALG, ID, A, E] =
    this.copy(createF = Some(f))

  def withUpdate(f: (ID, A) => Future[Either[E, ID]]): ClassicCrudDefinition[ALG, ID, A, E] =
    this.copy(updateF = Some(f))

  def withDelete(f: ID => Future[Either[E, (ID, A)]]): ClassicCrudDefinition[ALG, ID, A, E] =
    this.copy(deleteF = Some(f))

  def withRead(f: ID => Future[Either[E, (ID, A)]]): ClassicCrudDefinition[ALG, ID, A, E] =
    this.copy(readF = Some(f))

//  def routes: Route = {}

  def routeForGet(f: ID => Future[Either[E, (ID, A)]]): Route = {
    pathPrefix(urlPath / Remaining) { idString =>
      get {
        pathStringToId(idString) match {
          case Left(err) =>
            complete(StatusCodes.UnprocessableEntity, JsObject(("errorMessage", JsString(err))))
          case Right(id) => {
            onComplete(f(id)) {
              case Success(value) => {
                value match {
                  case Left(systemError) =>
                    complete(StatusCodes.InternalServerError, errorMarshallerFunction(systemError))
                  case Right(value) =>
                    complete(StatusCodes.OK, Seq.empty, schemaMarshallerFunction(value))
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

  def routeForCreate(f: A => Future[Either[E, ID]]): Route = {
    path(urlPath) {
      post {
        entity(as[Array[Byte]]) { bytes =>
          {
            val futureResult: Future[StandardRoute] = schemaUnmarshallerFunction(bytes) match {
              case Left(err) =>
                Future.apply(
                  complete(
                    StatusCodes.UnprocessableEntity,
                    validationErrorEncoder(ErrorResponse(err.toList))))
              case Right(value) =>
                f(value).map {
                  case Left(systemError) =>
                    complete(StatusCodes.InternalServerError, errorMarshallerFunction(systemError))
                  case Right(value) =>
                    complete(StatusCodes.OK, Seq.empty, idSchemaMarshallerFunction(value))
                }
            }
            Await.result(futureResult, 5.seconds)
          }
        }
      }
    }
  }
}
