package com.bones.http4s

import java.nio.charset.Charset
import java.util.UUID

import cats.data.{EitherT, NonEmptyList}
import cats.effect.Sync
import cats.syntax.all._
import com.bones.Util
import com.bones.bson.values.BsonScalaCoreValueEncoderInterpreter
import com.bones.bson.{BsonEncoderInterpreter, BsonValidatorInterpreter}
import com.bones.circe.values.BaseScalaCoreEncoder
import com.bones.circe.{
  CirceEncoderInterpreter,
  CirceValidatorInterpreter,
  IsoCirceEncoderInterpreter
}
import com.bones.data.Error.ExtractionError
import com.bones.data.KvpCollection.headManifest
import com.bones.data._
import com.bones.data.values.ScalaCoreValue
import com.bones.http4s.CrudInterpreterDescription._
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.protobuf._
import com.bones.protobuf.messageType.ProtoFileGeneratorInterpreter
import com.bones.protobuf.values.ProtobufScalaCoreEncoder
import com.bones.syntax._
import fs2.Stream
import io.circe.Json
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.util.CaseInsensitiveString

import scala.util.Try

object BaseCrudInterpreter {

  object StringToIdError {
    private val stringToIdErrorHList =
      ("input", string()) ::
        ("errorMessage", string()) ::
        kvpNil

    val stringToIdErrorSchema =
      stringToIdErrorHList.convert[StringToIdError]
  }
  case class StringToIdError(input: String, errorMessage: String)

  private val stringToIdErrorJsonEncoder =
    com.bones.circe.values.isoCirceEncoderInterpreter
      .generateEncoder(StringToIdError.stringToIdErrorSchema)
  private val stringToIdErrorBsonEncoder = com.bones.bson.values.defaultBsonEncoderInterpreter
    .generateEncoder(StringToIdError.stringToIdErrorSchema)
  private val stringToIdProtoEncoder =
    com.bones.protobuf.values.defaultEncoder
      .generateProtobufEncoder(StringToIdError.stringToIdErrorSchema)

  val intParam: String => Either[StringToIdError, Int] = param => {
    Try { param.toInt }.toOption
      .toRight(StringToIdError(param, s"Could not convert parameter '${param}' to Int"))
  }

  val longParam: String => Either[StringToIdError, Long] = param => {
    Util
      .stringToLong(param)
      .toRight(StringToIdError(param, s"Could not convert parameter '${param}' to Long"))
  }

  val uuidParam: String => Either[StringToIdError, UUID] = param => {
    Try { UUID.fromString(param) }.toOption
      .toRight(StringToIdError(param, s"Could not convert parameter '${param}' to UUID"))
  }

  def stringToIdErrorToResponse[F[_]](stringToIdError: StringToIdError, contentType: String)(
    implicit F: Sync[F],
    H: Http4sDsl[F]): F[Response[F]] = {
    import H._
    contentType match {
      case "application/ubjson" =>
        val bson = stringToIdErrorBsonEncoder(stringToIdError)
        val bytes = com.bones.bson.bsonResultToBytes(bson)
        H.BadRequest(bytes)
      case "application/protobuf" =>
        val bytes = stringToIdProtoEncoder(stringToIdError)
        H.BadRequest(bytes)
      case _ => // including application/json
        val str = stringToIdErrorJsonEncoder(stringToIdError).noSpaces
        H.BadRequest(str)
    }
  }

  object ErrorResponse {

    val error = ExtractionErrorEncoder.extractionErrorSchema
      .toSuperclassOf[ExtractionError](
        manifest[ExtractionError],
        ExtractionErrorEncoder.extractionErrorGeneric)

    private val errorResponseHList =
      ("errors", ListData[ScalaCoreValue, ExtractionError](Left(error.asValue), List.empty)) :<:
        new KvpNil[ScalaCoreValue]

    val errorResponseSchema = errorResponseHList.convert[ErrorResponse]
  }
  case class ErrorResponse(errors: List[ExtractionError])
  private val jsonEncoder =
    IsoCirceEncoderInterpreter(BaseScalaCoreEncoder)
      .generateEncoder(ErrorResponse.errorResponseSchema)

  private val bsonEncoder =
    BsonScalaCoreValueEncoderInterpreter.generateEncoder(ErrorResponse.errorResponseSchema)

  private val protoEncoder = {
    val encoder = new ProtobufSequentialEncoderInterpreter[ScalaCoreValue] {
      override val customInterpreter: ProtobufEncoderValue[ScalaCoreValue] =
        ProtobufScalaCoreEncoder
    }
    encoder.generateProtobufEncoder(ErrorResponse.errorResponseSchema)
  }

  /**
    * Return the error encoded based on the content type
    * @param ee The List of errors
    * @param contentType One of "application/ubjson", "application/protobuf", "application/json"
    * @param F Sync of F
    * @param H The Http4s DSL
    * @return Response with the errors encoded appropriately
    */
  def extractionErrorToResponse[F[_]](ee: NonEmptyList[ExtractionError], contentType: String)(
    implicit F: Sync[F],
    H: Http4sDsl[F]): F[Response[F]] = {
    // This import os for the EntityResponseGenerator
    import H._
    val errorResponse = ErrorResponse(ee.toList)
    contentType match {
      case "application/ubjson" =>
        val bson = bsonEncoder(errorResponse)
        val bytes = com.bones.bson.bsonResultToBytes(bson)
        H.BadRequest(bytes)
      case "application/protobuf" =>
        val bytes = protoEncoder(errorResponse)
        H.BadRequest(bytes)
      case "application/json" =>
        H.BadRequest(jsonEncoder(errorResponse).noSpaces)
      case _ =>
        val msg =
          s"unknown content type $contentType, reverting to 'application/json"
        val json = jsonEncoder(errorResponse).mapObject(obj =>
          obj.add("contentTypeError", Json.fromString(msg)))
        H.BadRequest(json.noSpaces)
    }
  }

  def schemaWithId[ALG[_], A, ID: Manifest](
    idDefinition: ALG[ID],
    schema: KvpCollection[ALG, A]) = {
    implicit def manifestA =
      headManifest(schema).getOrElse(
        throw new UnsupportedOperationException("No manifest for A available"))
    (("id", idDefinition) :: schema :: new KvpNil[ALG]).tupled[(ID, A)]
  }

  def httpDeleteRoutes[F[_], ALG[_], A, E, B, ID](
    path: String,
    pathStringToId: String => Either[StringToIdError, ID],
    del: ID => F[Either[E, B]],
    errorSchema: KvpCollection[ALG, E],
    outputSchema: KvpCollection[ALG, B],
    circeEncoderInterpreter: CirceEncoderInterpreter[ALG],
    bsonEncoder: BsonEncoderInterpreter[ALG],
    protobufEncoder: ProtobufSequentialEncoderInterpreter[ALG],
    charset: Charset)(implicit F: Sync[F], H: Http4sDsl[F]) = {
    val outputF =
      circeEncoderInterpreter.generateEncoder(outputSchema)
    val errorF =
      circeEncoderInterpreter.generateEncoder(errorSchema)
    val json = DeleteInterpreterGroup[E, B](
      "application/json",
      dout => outputF(dout).spaces2.getBytes(charset),
      de => errorF(de).spaces2.getBytes(charset)
    )

    val bOutputF =
      bsonEncoder.generateEncoder(outputSchema)
    val bErrorF =
      bsonEncoder.generateEncoder(errorSchema)
    val bson = DeleteInterpreterGroup[E, B](
      "application/ubjson",
      dout => com.bones.bson.bsonResultToBytes(bOutputF(dout)),
      de => com.bones.bson.bsonResultToBytes(bErrorF(de))
    )

    val pOutputF =
      protobufEncoder
        .generateProtobufEncoder(outputSchema)
    val pErrorF =
      protobufEncoder
        .generateProtobufEncoder(errorSchema)
    val protobuf = DeleteInterpreterGroup[E, B](
      "application/protobuf",
      pOutputF,
      pErrorF
    )

    delete(path, json, pathStringToId, del) ::
      delete(path, bson, pathStringToId, del) ::
      delete(path, protobuf, pathStringToId, del) ::
      Nil
  }

  /** Create delete routes from interpreter group */
  def delete[F[_], ALG[_], E, B, ID](
    expectedPath: String,
    interpreterGroup: DeleteInterpreterGroup[E, B],
    stringParamToId: String => Either[StringToIdError, ID],
    deleteF: ID => F[Either[E, B]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.DELETE -> Root / path / idParam
          if (expectedPath == path && contentType(req).contains(interpreterGroup.contentType)) =>
        stringParamToId(idParam)
          .leftMap(e => stringToIdErrorToResponse(e, interpreterGroup.contentType))
          .map(id => {
            deleteF(id).flatMap {
              case Right(entity) =>
                Ok(
                  interpreterGroup.outInterpreter(entity),
                  Header("Content-Type", interpreterGroup.contentType)
                )
              case Left(de) =>
                InternalServerError.apply(
                  interpreterGroup.errorInterpreter(de),
                  Header("Content-Type", interpreterGroup.contentType)
                )
            }
          })
          .merge
    }
  }

  def httpPostRoutes[F[_], ALG[_], A, E, B, ID](
    path: String,
    create: A => F[Either[E, B]],
    inputSchema: KvpCollection[ALG, A],
    errorSchema: KvpCollection[ALG, E],
    outputSchema: KvpCollection[ALG, B],
    validatedFromCirceInterpreter: CirceValidatorInterpreter[ALG],
    encodeToCirceInterpreter: CirceEncoderInterpreter[ALG],
    bsonValidator: BsonValidatorInterpreter[ALG],
    bsonEncoder: BsonEncoderInterpreter[ALG],
    protobufValidator: ProtobufSequentialValidatorInterpreter[ALG],
    protobufEncoder: ProtobufSequentialEncoderInterpreter[ALG],
    charset: Charset
  )(implicit F: Sync[F], H: Http4sDsl[F]) = {
    val inputF =
      validatedFromCirceInterpreter
        .generateByteArrayValidator(inputSchema, charset)
    val outputF =
      encodeToCirceInterpreter.generateEncoder(outputSchema)
    val errorF =
      encodeToCirceInterpreter.generateEncoder(errorSchema)

    val json = PutPostInterpreterGroup[A, E, B](
      "application/json",
      bytes => inputF(bytes),
      uo => outputF(uo).spaces2.getBytes(charset),
      ue => errorF(ue).spaces2.getBytes(charset)
    )

    val bInputF =
      bsonValidator.generateValidator(inputSchema)
    val bOutputF =
      bsonEncoder.generateEncoder(outputSchema)
    val bErrorF =
      bsonEncoder.generateEncoder(errorSchema)

    val bson = PutPostInterpreterGroup[A, E, B](
      "application/ubjson",
      byte =>
        com.bones.bson
          .fromByteArray(byte)
          .flatMap(bjson => bInputF(bjson)),
      co => com.bones.bson.bsonResultToBytes(bOutputF(co)),
      ce => com.bones.bson.bsonResultToBytes(bErrorF(ce))
    )

    val pInputF =
      protobufValidator.fromCustomBytes(inputSchema)
    val pOutputF = protobufEncoder
      .generateProtobufEncoder(outputSchema)
    val pErrorF =
      protobufEncoder
        .generateProtobufEncoder(errorSchema)
    val protoBuf = PutPostInterpreterGroup[A, E, B](
      "application/protobuf",
      pInputF,
      pOutputF,
      pErrorF
    )

    post(path, json, create) ::
      post(path, bson, create) ::
      post(path, protoBuf, create) :: Nil

  }

  /** Create the post endpoint from the Interpreter Group */
  def post[F[_], A, E, B, ID](
    expectedPath: String,
    interpreterGroup: PutPostInterpreterGroup[A, E, B],
    createF: A => F[Either[E, B]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path
          if (expectedPath == path && contentType(req).contains(interpreterGroup.contentType)) =>
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup
              .inInterpreter(body)
              .left
              .map(x => extractionErrorToResponse(x, interpreterGroup.contentType))
          }
          out <- EitherT[F, F[Response[F]], B] {
            createF(in)
              .map(_.left.map(ce => {
                val out = interpreterGroup.errorInterpreter(ce)
                InternalServerError(out, Header("Content-Type", interpreterGroup.contentType))
              }))
          }
        } yield {
          Ok(
            interpreterGroup.outInterpreter(out),
            Header("Content-Type", interpreterGroup.contentType)
          )
        }
        result.value.flatMap(_.merge)
    }
  }

  def httpGetRoute[F[_], ALG[_], E, B, ID](
    path: String,
    pathStringToId: String => Either[StringToIdError, ID],
    read: ID => F[Either[E, B]],
    errorSchema: KvpCollection[ALG, E],
    outputSchema: KvpCollection[ALG, B],
    jsonEncoder: CirceEncoderInterpreter[ALG],
    bsonEncoder: BsonEncoderInterpreter[ALG],
    protobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter[ALG],
    charset: Charset)(implicit F: Sync[F], H: Http4sDsl[F]) = {
    val outputF =
      jsonEncoder.generateEncoder(outputSchema)
    val errorF =
      jsonEncoder.generateEncoder(errorSchema)
    val json = GetInterpreterGroup[E, B](
      "application/json",
      ro => {
        outputF(ro).spaces2.getBytes(charset)
      },
      re => errorF(re).spaces2.getBytes(charset)
    )

    val bOutputF =
      bsonEncoder.generateEncoder(outputSchema)
    val bErrorF =
      bsonEncoder.generateEncoder(errorSchema)
    val bson = GetInterpreterGroup[E, B](
      "application/ubjson",
      ro => com.bones.bson.bsonResultToBytes(bOutputF(ro)),
      re => com.bones.bson.bsonResultToBytes(bErrorF(re))
    )

    val pOutputF = protobufSequentialOutputInterpreter
      .generateProtobufEncoder(outputSchema)
    val pErrorF =
      protobufSequentialOutputInterpreter
        .generateProtobufEncoder(errorSchema)
    val protoBuf = GetInterpreterGroup[E, B](
      "application/protobuf",
      pOutputF,
      pErrorF
    )

    get(path, json, pathStringToId, read) ::
      get(path, bson, pathStringToId, read) ::
      get(path, protoBuf, pathStringToId, read) ::
      Nil
  }

  /**
    * Create a get endpoint.
    */
  def get[F[_], E, B, ID](
    expectedPath: String,
    interpreterGroup: GetInterpreterGroup[E, B],
    stringParamToId: String => Either[StringToIdError, ID],
    readF: ID => F[Either[E, B]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam
          if expectedPath == path && contentType(req).contains(interpreterGroup.contentType) =>
        stringParamToId(idParam)
          .leftMap(e => stringToIdErrorToResponse(e, interpreterGroup.contentType))
          .map(id => {
            readF(id)
              .flatMap({
                case Left(re) =>
                  BadRequest.apply[Array[Byte]](
                    interpreterGroup.errorInterpreter(re),
                    Header("Content-Type", interpreterGroup.contentType)
                  )(F, entityEncoder)
                case Right(ro) =>
                  Ok(
                    interpreterGroup.outInterpreter(ro),
                    Header("Content-Type", interpreterGroup.contentType)
                  )
              })
          })
          .merge
    }
  }

  /**
    * Create a PUT endpoint given serialization functors and business logic.
    */
  def updateRoute[F[_], ALG[_], A, E, B, ID](
    path: String,
    pathStringToId: String => Either[StringToIdError, ID],
    updateF: (ID, A) => F[Either[E, B]],
    inputSchema: KvpCollection[ALG, A],
    errorSchema: KvpCollection[ALG, E],
    outputSchema: KvpCollection[ALG, B],
    jsonValidator: CirceValidatorInterpreter[ALG],
    jsonEncoder: CirceEncoderInterpreter[ALG],
    bsonValidator: BsonValidatorInterpreter[ALG],
    bsonEncoder: BsonEncoderInterpreter[ALG],
    protobufValidator: ProtobufSequentialValidatorInterpreter[ALG],
    protobufEncoder: ProtobufSequentialEncoderInterpreter[ALG],
    charset: Charset
  )(implicit F: Sync[F], H: Http4sDsl[F]): List[HttpRoutes[F]] = {
    val inputValidation =
      jsonValidator
        .generateByteArrayValidator(inputSchema, charset)
    val outputEncoder =
      jsonEncoder.generateEncoder(outputSchema)
    val errorEncoder =
      jsonEncoder.generateEncoder(errorSchema)

    val jsonPut = PutPostInterpreterGroup[A, E, B](
      "application/json",
      inputValidation,
      uo => outputEncoder(uo).spaces2.getBytes(charset),
      ue => errorEncoder(ue).spaces2.getBytes(charset)
    )

    val bInputValidation =
      bsonValidator.generateValidator(inputSchema)
    val bOutputEncoder =
      bsonEncoder.generateEncoder(outputSchema)
    val bErrorEncoder =
      bsonEncoder.generateEncoder(errorSchema)

    val bsonPut = PutPostInterpreterGroup[A, E, B](
      "application/ubjson",
      byte =>
        com.bones.bson
          .fromByteArray(byte)
          .flatMap(bjson => bInputValidation(bjson)),
      uo => com.bones.bson.bsonResultToBytes(bOutputEncoder(uo)),
      ue => com.bones.bson.bsonResultToBytes(bErrorEncoder(ue))
    )

    val pInputInterpreter =
      protobufValidator
        .fromCustomBytes(inputSchema)
    val pOutputEncoder = protobufEncoder
      .generateProtobufEncoder(outputSchema)
    val protobufErrorEncoder = protobufEncoder
      .generateProtobufEncoder(errorSchema)

    val protoBufPut = PutPostInterpreterGroup[A, E, B](
      "application/protobuf",
      bytes => pInputInterpreter(bytes),
      uo => pOutputEncoder(uo),
      ue => protobufErrorEncoder(ue)
    )

    put(path, jsonPut, pathStringToId, updateF) ::
      put(path, bsonPut, pathStringToId, updateF) ::
      put(path, protoBufPut, pathStringToId, updateF) ::
      Nil
  }

  /**
    * Create a PUT endpoint given serialization functors and business logic.
    *
    * @param interpreterGroup contains functions to and from Array[Byte]
    * @param updateF          Business logic to execute after
    * @return
    */
  def put[F[_], ALG[_], A, E, B, ID](
    expectedPath: String,
    interpreterGroup: PutPostInterpreterGroup[A, E, B],
    stringParamToId: String => Either[StringToIdError, ID],
    updateF: (ID, A) => F[Either[E, B]]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._

    HttpRoutes.of[F] {
      case req @ Method.PUT -> Root / path / idParam
          if expectedPath == path && contentType(req).contains(interpreterGroup.contentType) =>
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          id <- EitherT.fromEither[F] {
            stringParamToId(idParam).leftMap(e =>
              stringToIdErrorToResponse(e, interpreterGroup.contentType))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup
              .inInterpreter(body)
              .left
              .map(x => extractionErrorToResponse(x, interpreterGroup.contentType))
          }
          out <- EitherT[F, F[Response[F]], B] {
            updateF
              .apply(id, in)
              .map(_.left.map(ce => InternalServerError(interpreterGroup.errorInterpreter(ce))))
          }
        } yield {
          Ok(
            interpreterGroup.outInterpreter(out),
            Header("Content-Type", interpreterGroup.contentType)
          )
        }
        result.value.flatMap(_.merge)
    }
  }

  def httpSearch[F[_], ALG[_], E, B](
    path: String,
    searchF: () => Stream[F, B],
    outputSchema: KvpCollection[ALG, B],
    jsonEncoder: CirceEncoderInterpreter[ALG],
    charset: Charset
  )(implicit F: Sync[F], H: Http4sDsl[F]) = {
    val outputF =
      jsonEncoder.generateEncoder(outputSchema)

    val jsonSearch = SearchInterpreterGroup[F, B](
      "application/json",
      ro => {
        Stream("[".getBytes(charset)) ++
          ro.map(out => outputF(out).noSpaces.getBytes(charset))
            .intersperse(",".getBytes(charset)) ++
          Stream("]".getBytes(charset))
      }
    )
    search(path, jsonSearch, searchF) :: Nil
  }

  /** Create search endpoints form the Interpreter Group */
  def search[F[_], ALG[_], E, B](
    path: String,
    interpreterGroup: SearchInterpreterGroup[F, B],
    searchF: () => fs2.Stream[F, B]
  )(implicit F: Sync[F], H: Http4sDsl[F]): HttpRoutes[F] = {
    import H._
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path
          if contentType(req).contains(interpreterGroup.contentType) =>
        Ok(
          interpreterGroup.outInterpreter(searchF()),
          Header("Content-Type", "application/json")
        )
    }
  }

  /** Get content type from the Request Headers if it exists */
  def contentType[F[_]](req: Request[F]): Option[String] =
    req.headers
      .find(header => header.name == CaseInsensitiveString("Content-Type"))
      .map(_.value)

}
trait BaseCrudInterpreter[ALG[_], A, E, B, F[_], ID] extends Http4sDsl[F] {

  def htmlEndpoint(customPath: Path, html: String)(implicit F: Sync[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case GET -> Root / path if customPath.toList == Path(path).toList =>
        Ok(html, Header("Content-Type", "text/html"))(F, implicitly[EntityEncoder[F, String]])
    }

  /** Create an endpoint to display the protobuf schema for each endpoint */
  def protoBuff(
    path: String,
    customProtobufInterpreter: ProtoFileGeneratorInterpreter[ALG],
    schema: KvpCollection[ALG, A],
    schemaWithId: KvpCollection[ALG, (ID, A)],
    errorSchema: KvpCollection[ALG, E]
  )(implicit F: Sync[F]): HttpRoutes[F] = {
    def toFile[B] =
      customProtobufInterpreter
        .fromSchemaToProtoFile(_: KvpCollection[ALG, B])

    val text =
      s"""
         | // Base Schema, Used for input on Create Only
         | ${toFile(schema)}
         |
         | // Base Schema with ID, Used for other CRUD operations besides input on Create
         | ${toFile(schemaWithId)}
         |
         | // Error Output Message
         | ${toFile(errorSchema)}
         |
        """.stripMargin

    HttpRoutes.of[F] {
      case GET -> Root / "proto" / path =>
        Ok(text, Header("Content-Type", "text/plain"))
    }
  }

}
