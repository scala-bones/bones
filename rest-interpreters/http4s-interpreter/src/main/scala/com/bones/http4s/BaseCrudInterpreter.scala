package com.bones.http4s

import cats.data.{EitherT, NonEmptyList}
import cats.effect.Sync
import com.bones.data.BonesSchema
import com.bones.data.Error.ExtractionError
import com.bones.protobuf.{ProtoFileGeneratorInterpreter, ProtobufUtcSequentialEncoderAndValidator}
import io.circe.Json
import org.http4s.{EntityEncoder, Header, HttpRoutes, Method, Request, Response}
import org.http4s.dsl.Http4sDsl
import org.http4s.util.CaseInsensitiveString
import io.circe.syntax._
import cats.implicits._
import com.bones.bson.BsonEncoderInterpreter
import com.bones.circe.IsoCirceEncoderAndValidatorInterpreter
import com.bones.data.custom.ExtractionErrorValue
import com.bones.interpreter.custom.ExtractionErrorEncoder
import shapeless.Generic
import org.http4s.circe._


trait BaseCrudInterpreter[ALG[_], A, E, F[_], ID] extends Http4sDsl[F] {

  import ClassicCrudInterpreter._
  import CrudInterpreterDescription._

  object ErrorResponse {
    import com.bones.syntax._

    private val errorResponseHlist =
      ("errors",
        list(
          ExtractionErrorEncoder.extractionErrorSchema
            .convert[ExtractionError](manifest[ExtractionError], ExtractionErrorEncoder.extractionErrorGeneric)
        )
      ) :<: kvpNilCov[ExtractionErrorValue]

    val errorResponseSchema = errorResponseHlist.convert[ErrorResponse]
  }
  case class ErrorResponse(errors: List[ExtractionError])

  val jsonEncoder = IsoCirceEncoderAndValidatorInterpreter.encoderFromCustomSchema(ErrorResponse.errorResponseSchema, com.bones.circe.custom.BaseExtractionErrorEncoder)
  val bsonEncoder = BsonEncoderInterpreter.encoderFromCustomSchema(ErrorResponse.errorResponseSchema, com.bones.bson.custom.DefaultBsonErrorEncoder)
  val protoEncoder = ProtobufUtcSequentialEncoderAndValidator.encodeToBytesCustomAlgebra(ErrorResponse.errorResponseSchema, com.bones.protobuf.custom.ExtractionErrorProtoEncoder)

  def eeToOut(ee: NonEmptyList[ExtractionError], contentType: String)(implicit F: Sync[F], H: Http4sDsl[F]): F[Response[F]] = {
    val errorResponse = ErrorResponse(ee.toList)
    contentType match {
      case "application/ubjson" => {
        val bson = bsonEncoder(errorResponse)
        val bytes = BsonEncoderInterpreter.bsonResultToBytes(bson)
        H.BadRequest(bytes)
      }
      case "application/protobuf" => {
        val bytes = protoEncoder.apply(errorResponse)
        H.BadRequest(bytes)
      }
      case "application/json" => H.BadRequest(jsonEncoder(errorResponse).noSpaces)
      case _ => {
        val msg = s"unknown content type $contentType, reverting to 'application/json"
        val json = jsonEncoder(errorResponse).mapObject(obj => obj.add("contentTypeError", Json.fromString(msg)))
        H.BadRequest(json.noSpaces)
      }
    }
  }

  def htmlEndpoint(customPath: Path, html: String)(implicit F: Sync[F]) =
    HttpRoutes.of[F] {
      case GET -> Root / customPath / path =>
        Ok(html, Header("Content-Type", "text/html"))(F, implicitly[EntityEncoder[F, String]])
    }




  /** Create an endpoint to display the protobuf schema for each endpoint */
  def protoBuff(
                 path: String,
                 customProtobufInterpreter: ProtobufEncoderInterpreter[ALG],
                 schema: BonesSchema[ALG, A],
                 schemaWithId: BonesSchema[ALG, (ID, A)],
                 errorSchema: BonesSchema[ALG, E]
               )(implicit F: Sync[F]): HttpRoutes[F] = {
    def toFile[A] =
      ProtoFileGeneratorInterpreter
        .fromSchemaToProtoFile[ALG, A](_: BonesSchema[ALG, A], customProtobufInterpreter)

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

  /** Crate delete routes from interpreter group */
  def delete[DO](
                  path: String,
                  interpreterGroup: DeleteInterpreterGroup[DO, E],
                  stringParamToId: String => Either[E, ID],
                  deleteF: ID => F[Either[E, DO]]
                )(implicit F: Sync[F]): HttpRoutes[F] = HttpRoutes.of[F] {
    case req @ Method.DELETE -> Root / path / idParam => {
      val result = for {
        id <- EitherT.fromEither[F] {
          stringParamToId(idParam)
        }
        entity <- EitherT[F, E, DO] {
          deleteF(id)
        }
      } yield Ok(
        interpreterGroup.outInterpreter(entity),
        Header("Content-Type", interpreterGroup.contentType)
      )

      result.value.flatMap(either => {
        either.left
          .map(
            de =>
              InternalServerError(
                interpreterGroup.errorInterpreter(de),
                Header("Content-Type", interpreterGroup.contentType)
              )
          )
          .merge
      })
    }
  }

  /** Get content type from the Request Headers if it exists */
  def contentType(req: Request[F]): Option[String] =
    req.headers
      .find(header => header.name == CaseInsensitiveString("Content-Type"))
      .map(_.value)

  /** Create search endpoints form the Interpreter Group */
  def search[CO](
                  path: String,
                  interpreterGroup: SearchInterpreterGroup[F, CO],
                  searchF: () => fs2.Stream[F, CO]
                )(implicit F: Sync[F]): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path
        if contentType(req).contains(interpreterGroup.contentType) => {
        Ok(
          interpreterGroup.outInterpreter(searchF()),
          Header("Content-Type", "application/json")
        )
      }
    }
  }

  /** Create the post endpoint from the Interpreter Group */
  def post[CI, CO, CE](
                        path: String,
                        interpreterGroup: PutPostInterpreterGroup[CI, CO, CE],
                        createF: CI => F[Either[CE, CO]]
                      )(implicit F: Sync[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ Method.POST -> Root / path
        if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x, interpreterGroup.contentType)(F, this))
          }
          out <- EitherT[F, F[Response[F]], CO] {
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

  /**
   * Create a get endpoint.
   */
  def get(
           path: String,
           interpreterGroup: GetInterpreterGroup[(ID, A), E],
           stringParamToId: String => Either[E, ID],
           readF: ID => F[Either[E, (ID, A)]]
         )(implicit F: Sync[F]): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req @ Method.GET -> Root / path / idParam
        if contentType(req).contains(interpreterGroup.contentType) => {
        stringParamToId(idParam).left
          .map(re => {
            val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
            BadRequest.apply[Array[Byte]](
              interpreterGroup.errorInterpreter(re),
              Header("Content-Type", interpreterGroup.contentType)
            )(F, entityEncoder)
          })
          .map(id => {
            readF(id)
              .flatMap({
                case Left(re) =>
                  val entityEncoder = implicitly[EntityEncoder[F, Array[Byte]]]
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
  }

  /**
   * Create a PUT endpoint given serialization functors and business logic.
   *
   * @param interpreterGroup contains functions to and from Array[Byte]
   * @param updateF          Business logic to execute after
   * @return
   */
  def put(
           path: String,
           interpreterGroup: PutPostInterpreterGroup[(ID, A), (ID, A), E],
           stringToId: String => Either[E, ID],
           updateF: (ID, A) => F[Either[E, (ID, A)]]
         )(implicit F: Sync[F]): HttpRoutes[F] = {

    HttpRoutes.of[F] {
      case req @ Method.PUT -> Root / path / idParam
        if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          id <- EitherT.fromEither[F] {
            stringToId(idParam).left.map(
              e =>
                BadRequest(
                  interpreterGroup.errorInterpreter(e),
                  Header("Content-Type", interpreterGroup.contentType)
                )
            )
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x, interpreterGroup.contentType)(F, this))
          }
          out <- EitherT[F, F[Response[F]], (ID, A)] {
            updateF
              .tupled(in)
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
  }
}
