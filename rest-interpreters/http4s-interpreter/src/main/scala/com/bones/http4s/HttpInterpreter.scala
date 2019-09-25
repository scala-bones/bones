package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.data.{EitherT, NonEmptyList}
import cats.effect._
import cats._
import cats.implicits._
import cats.data._
import cats.kernel.Semigroup
import com.bones.bson.{EncodeToBson, ValidatedFromBsonInterpreter}
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra._
import com.bones.data.Error.ExtractionError
import com.bones.data.Value.BonesSchema
import com.bones.oas3.CrudOasInterpreter
import com.bones.protobuf.{ProtoFileInterpreter, ProtobufSequentialInputInterpreter, ProtobufSequentialOutputInterpreter}
import fs2.Stream
import io.circe.syntax._
import io.circe.{Json, ParsingFailure}
import io.swagger.v3.oas.models.OpenAPI
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
//import org.http4s.dsl.io._
import org.http4s.util.CaseInsensitiveString
import org.http4s._

case class HttpInterpreter(
                            charset: java.nio.charset.Charset = StandardCharsets.UTF_8) {

  import HttpInterpreter._

  val encodeToCirceInterpreter = EncodeToCirceInterpreter.isoInterpreter
  val validatedFromCirceInterpreter = ValidatedFromCirceInterpreter.isoInterpreter

  case class DataTransformation[I, O, E](description: String,
                                         f: I => Either[E, O])

  /**
    * Creates an Http4s Routs for the given input.
    *
    * @param serviceOps Defines what crud operations are available.
    * @param searchF    Business logic function for search.
    * @param createF    Business logic function for create.
    * @param readF      Business logic functions for read.
    * @param updateF    Business logic function for update.
    * @param deleteF    Business logic function for a delete.
    * @tparam CI Create Input Type (Usually without an ID)
    * @tparam CO Create Output Type (Usually with an ID)
    * @tparam CE Create Error type.
    * @tparam RO Read Input Type ( Usually with an ID, probably the same as CO)
    * @tparam RE Read Error
    * @tparam UI Update Input
    * @tparam UO Update Output
    * @tparam UE Update Error
    * @tparam DO Delete Output
    * @tparam DE Delete Error
    * @return HttpRoutes which can be fed to Http4s.
    */
  def forService[F[_], CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
                                                                serviceOps: ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE],
                                                                createF: CI => F[Either[CE, CO]],
                                                                readF: Long => F[Either[RE, RO]],
                                                                searchF: Stream[F, RO],
                                                                updateF: (Long, UI) => F[Either[UE, UO]],
                                                                deleteF: Long => F[Either[DE, DO]])(
                                                                implicit F: Sync[F]
                                                              ): HttpRoutes[F] = {

    val path = serviceOps.path

    object http extends HttpInterpreterF[F]

    val updateHttpService =
      serviceOps.updateOperation.toList.flatMap(update => {
        val inputF =
          validatedFromCirceInterpreter.fromSchema(update.inputSchema)
        val outputF = encodeToCirceInterpreter.fromSchema(update.outputSchema)
        val errorF = encodeToCirceInterpreter.fromSchema(update.failureSchema)

        val json = PutPostInterpreterGroup[UI, UO, UE](
          "application/json",
          bytes =>
            validatedFromCirceInterpreter
              .fromByteArray(bytes, charset)
              .flatMap(json => inputF(json, List.empty)),
          uo => outputF(uo).spaces2.getBytes(charset),
          ue => errorF(ue).spaces2.getBytes(charset)
        )

        val bInputF =
          ValidatedFromBsonInterpreter.fromSchema(update.inputSchema)
        val bOutputF = EncodeToBson.fromSchema(update.outputSchema)
        val bErrorF = EncodeToBson.fromSchema(update.failureSchema)

        val bson = PutPostInterpreterGroup[UI, UO, UE](
          "application/ubjson",
          byte =>
            ValidatedFromBsonInterpreter
              .fromByteArray(byte)
              .flatMap(bjson => bInputF(bjson, List.empty)),
          uo => EncodeToBson.bsonResultToBytes(bOutputF(uo)),
          ue => EncodeToBson.bsonResultToBytes(bErrorF(ue))
        )

        val pInputF =
          ProtobufSequentialInputInterpreter.fromBytes(update.inputSchema)
        val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(
          update.outputSchema)
        val pErrorF = ProtobufSequentialOutputInterpreter.encodeToBytes(
          update.failureSchema)

        val protoBuf = PutPostInterpreterGroup[UI, UO, UE](
          "application/protobuf",
          bytes => pInputF(bytes),
          uo => pOutputF(uo),
          ue => pErrorF(ue)
        )

        http.put(path, json, updateF) :: http.put(path, bson, updateF) :: http.put(
          path,
          protoBuf,
          updateF) :: Nil

      })

    val readHttpService = serviceOps.readOperation.toList.flatMap(read => {
      val outputF =
        encodeToCirceInterpreter.fromSchema(read.outputSchema)
      val errorF = encodeToCirceInterpreter.fromSchema(read.errorSchema)
      val json = GetInterpreterGroup[RO, RE](
        "application/json",
        ro => outputF(ro).spaces2.getBytes(charset),
        re => errorF(re).spaces2.getBytes(charset)
      )

      val bOutputF = EncodeToBson.fromSchema(read.outputSchema)
      val bErrorF = EncodeToBson.fromSchema(read.errorSchema)
      val bson = GetInterpreterGroup[RO, RE](
        "application/ubjson",
        ro => EncodeToBson.bsonResultToBytes(bOutputF(ro)),
        re => EncodeToBson.bsonResultToBytes(bErrorF(re))
      )

      val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(
        read.outputSchema)
      val pErrorF =
        ProtobufSequentialOutputInterpreter.encodeToBytes(read.errorSchema)
      val protoBuf = GetInterpreterGroup[RO, RE](
        contentType = "application/protobuf",
        pOutputF,
        pErrorF
      )

      val jsonSearch = SearchInterpreterGroup[F,RO](
        "application/json",
        ro => {
          Stream("[".getBytes(charset)) ++
            ro.map(out => outputF(out).asJson.noSpaces.getBytes(charset))
              .intersperse(",".getBytes(charset)) ++
            Stream("]".getBytes(charset))
        }
      )

      http.get(path, json, readF) :: http.get(path, bson, readF) :: http.get(
        path,
        protoBuf,
        readF) :: http.search(path, jsonSearch, searchF) :: Nil

    })

    val createHttpService =
      serviceOps.createOperation.toList.flatMap(create => {
        val inputF =
          validatedFromCirceInterpreter.fromSchema(create.inputSchema)
        val outputF = encodeToCirceInterpreter.fromSchema(create.outputSchema)
        val errorF = encodeToCirceInterpreter.fromSchema(create.errorSchema)

        val json = PutPostInterpreterGroup[CI, CO, CE](
          "application/json",
          bytes =>
            validatedFromCirceInterpreter
              .fromByteArray(bytes, charset)
              .flatMap(json => inputF(json, List.empty)),
          uo => outputF(uo).spaces2.getBytes(charset),
          ue => errorF(ue).spaces2.getBytes(charset)
        )

        val bInputF =
          ValidatedFromBsonInterpreter.fromSchema(create.inputSchema)
        val bOutputF = EncodeToBson.fromSchema(create.outputSchema)
        val bErrorF = EncodeToBson.fromSchema(create.errorSchema)

        val bson = PutPostInterpreterGroup[CI, CO, CE](
          "application/ubjson",
          byte =>
            ValidatedFromBsonInterpreter
              .fromByteArray(byte)
              .flatMap(bjson => bInputF(bjson, List.empty)),
          co => EncodeToBson.bsonResultToBytes(bOutputF(co)),
          ce => EncodeToBson.bsonResultToBytes(bErrorF(ce))
        )

        val pInputF =
          ProtobufSequentialInputInterpreter.fromBytes(create.inputSchema)
        val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(
          create.outputSchema)
        val pErrorF =
          ProtobufSequentialOutputInterpreter.encodeToBytes(create.errorSchema)
        val protoBuf = PutPostInterpreterGroup[CI, CO, CE](
          "application/protobuf",
          pInputF,
          pOutputF,
          pErrorF
        )

        http.post(path, json, createF) :: http.post(path, bson, createF) :: http.post(
          path,
          protoBuf,
          createF) :: Nil

      })

    val deleteHttpService =
      serviceOps.deleteOperation.toList.flatMap(del => {
        val outputF = encodeToCirceInterpreter.fromSchema(del.outputSchema)
        val errorF = encodeToCirceInterpreter.fromSchema(del.errorSchema)
        val json = DeleteInterpreterGroup[DO, DE](
          "application/json",
          dout => outputF(dout).spaces2.getBytes(charset),
          de => errorF(de).spaces2.getBytes(charset)
        )

        val bOutputF = EncodeToBson.fromSchema(del.outputSchema)
        val bErrorF = EncodeToBson.fromSchema(del.errorSchema)
        val bson = DeleteInterpreterGroup[DO, DE](
          "application/ubjson",
          dout => EncodeToBson.bsonResultToBytes(bOutputF(dout)),
          de => EncodeToBson.bsonResultToBytes(bErrorF(de))
        )

        val pOutputF =
          ProtobufSequentialOutputInterpreter.encodeToBytes(del.outputSchema)
        val pErrorF =
          ProtobufSequentialOutputInterpreter.encodeToBytes(del.errorSchema)
        val protobuf = DeleteInterpreterGroup[DO, DE](
          "application/protobuf",
          pOutputF,
          pErrorF
        )

        http.delete(path, json, deleteF) :: http.delete(path, bson, deleteF) :: http.delete(
          path,
          protobuf,
          deleteF) :: Nil

      })

    val contentTypes = "application/json" :: "application/ubjson" :: "application/protobuf" :: Nil
    val swagger = http.swaggerDoc(contentTypes, serviceOps)

    val services: List[HttpRoutes[F]] =
      http.protoBuff(serviceOps) :: swagger :: createHttpService ::: readHttpService ::: updateHttpService ::: deleteHttpService

    services.foldLeft[HttpRoutes[F]](HttpRoutes.empty)(
      (op1: HttpRoutes[F], op2: HttpRoutes[F]) => op1 <+> op2
    )

  }

}

object HttpInterpreter {
  /** Collection of items used to to create a Post endpoint in this HttpInterpreter. */
  case class PutPostInterpreterGroup[UI, UO, UE](
                                                  contentType: String,
                                                  inInterpreter: Array[Byte] => Either[NonEmptyList[ExtractionError], UI],
                                                  outInterpreter: UO => Array[Byte],
                                                  errorInterpreter: UE => Array[Byte]
                                                )

  /** Collection of items used to to get a Post endpoint in this HttpInterpreter. */
  case class GetInterpreterGroup[RO, RE](
                                          contentType: String,
                                          outInterpreter: RO => Array[Byte],
                                          errorInterpreter: RE => Array[Byte]
                                        )

  /** Collection of items used to to create a Delete endpoint in this HttpInterpreter. */
  case class DeleteInterpreterGroup[DO, DE](
                                             contentType: String,
                                             outInterpreter: DO => Array[Byte],
                                             errorInterpreter: DE => Array[Byte]
                                           )

  /** Collection of items used to to create a Search endpoint in this HttpInterpreter. */
  case class SearchInterpreterGroup[F[_],SO](
                                         contentType: String,
                                         outInterpreter: Stream[F, SO] => Stream[F, Array[Byte]]
                                       )
}

trait HttpInterpreterF[F[_]] extends Http4sDsl[F] {

  import HttpInterpreter._

  def eeToOut(ee: NonEmptyList[ExtractionError])(implicit F: Sync[F]): F[Response[F]] = {

    val errors = ee.map(_.toString)
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }

  /** Create an endpoint to display the swagger doc for for this type.  Endpoint is /swagger/'entityName' */
  def swaggerDoc(
                  contentTypes: List[String],
                  serviceOps: ServiceOps[_, _, _, _, _, _, _, _, _, _])(implicit F: Sync[F]): HttpRoutes[F] = {

    val openApi =
      CrudOasInterpreter.jsonApiForService("Rest Service", "1.0", contentTypes, serviceOps)(
        new OpenAPI())
    val html = io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)

    HttpRoutes.of[F] {
      case GET -> Root / "swagger" / serviceOps.path =>
        Ok(html, Header("Content-Type", "text/html"))(F,implicitly[EntityEncoder[F,String]])
    }
  }

  /** Create an endpoint to dispaly the protobuf schema for each endpoint */
  def protoBuff(
                 serviceOps: ServiceOps[_, _, _, _, _, _, _, _, _, _])(
                       implicit F: Sync[F]): HttpRoutes[F] = {
    def toFile[A] =
      ProtoFileInterpreter.fromSchemaToProtoFile(_: BonesSchema[A])

    val createProto = serviceOps.createOperation
      .map(algebra => {
        s"""
           | // Create Input Message
           | ${toFile(algebra.inputSchema)}
           |
          | // Create Successful Output Message
           | ${toFile(algebra.outputSchema)}
           |
          | // Create Error Output Message
           | ${toFile(algebra.errorSchema)}
           |
        """.stripMargin
      })
      .getOrElse("")

    val readProto = serviceOps.readOperation
      .map(algebra => {
        s"""
           |
         | // Read Successful Output Message
           | ${toFile(algebra.outputSchema)}
           |
         | // Read Error Output Message
           | ${toFile(algebra.errorSchema)}
           |
        """.stripMargin
      })
      .getOrElse("")

    val updateProto = serviceOps.updateOperation
      .map(algebra => {
        s"""
           | // Update Input Message
           | ${toFile(algebra.inputSchema)}
           |
         | // Update Successful Output Message
           | ${toFile(algebra.outputSchema)}
           |
         | // Update Error Output Message
           | ${toFile(algebra.failureSchema)}
           |
        """.stripMargin
      })
      .getOrElse("")

    val deleteProto = serviceOps.deleteOperation
      .map(algebra => {
        s"""
           | // Delete Successful Output Message
           | ${toFile(algebra.outputSchema)}
           |
         | // Delete Error Output Message
           | ${toFile(algebra.errorSchema)}
           |
        """.stripMargin
      })
      .getOrElse("")

    val text = createProto + readProto + updateProto + deleteProto

    HttpRoutes.of[F] {
      case GET -> Root / "proto" / serviceOps.path =>
        Ok(text, Header("Content-Type", "text/plain"))
    }

  }

  /** Crate delete routes from interpreter group */
  def delete[DO, DE](
                      path: String,
                      interpreterGroup: DeleteInterpreterGroup[DO, DE],
                      deleteF: Long => F[Either[DE, DO]]
                    )(implicit F: Sync[F]): HttpRoutes[F] = HttpRoutes.of[F] {
    case req@Method.DELETE -> Root / path / LongVar(id) => {
      val result = for {
        entity <- EitherT[F, F[Response[F]], DO] {
          deleteF(id).map(
            _.left.map(
              de =>
                InternalServerError(interpreterGroup.errorInterpreter(de),
                  Header("Content-Type",
                    interpreterGroup.contentType))))
        }
      } yield
        Ok(interpreterGroup.outInterpreter(entity),
          Header("Content-Type", interpreterGroup.contentType))

      result.value.flatMap(_.merge)
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
                  interpreterGroup: SearchInterpreterGroup[F,CO],
                  searchF: fs2.Stream[F, CO]
                )(implicit F: Sync[F]): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req@Method.GET -> Root / path
        if contentType(req).contains(interpreterGroup.contentType) => {
        Ok(
          interpreterGroup.outInterpreter(searchF),
          Header("Content-Type", "application/json")
        )
      }
    }
  }

  /** Crate the post endpoint from the Interpreter Group */
  def post[CI, CO, CE](
                        path: String,
                        interpreterGroup: PutPostInterpreterGroup[CI, CO, CE],
                        createF: CI => F[Either[CE, CO]]
                      )(implicit F: Sync[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req@Method.POST -> Root / path
        if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x))
          }
          out <- EitherT[F, F[Response[F]], CO] {
            createF(in)
              .map(_.left.map(ce => {
                val out = interpreterGroup.errorInterpreter(ce)
                InternalServerError(out,
                  Header("Content-Type",
                    interpreterGroup.contentType))
              }))
          }
        } yield {
          Ok(interpreterGroup.outInterpreter(out),
            Header("Content-Type", interpreterGroup.contentType))
        }
        result.value.flatMap(_.merge)
      }
    }

  /**
    * Create a get endpoint.
    */
  def get[RO, RE](path: String,
                        interpreterGroup: GetInterpreterGroup[RO, RE],
                        readF: Long => F[Either[RE, RO]])(
                         implicit F: Sync[F]): HttpRoutes[F] = {
    HttpRoutes.of[F] {
      case req@Method.GET -> Root / path / LongVar(id)
        if contentType(req).contains(interpreterGroup.contentType) => {
        readF(id)
          .flatMap({
            case Left(re) =>
              val entityEncoder = implicitly[EntityEncoder[F,Array[Byte]]]
              BadRequest.apply[Array[Byte]](interpreterGroup.errorInterpreter(re),
                Header("Content-Type", interpreterGroup.contentType))(F,entityEncoder)
            case Right(ro) =>
              Ok(interpreterGroup.outInterpreter(ro),
                Header("Content-Type", interpreterGroup.contentType))
          })
      }
    }
  }

  /**
    * Create a PUT endpoint given serialization functors and business logic.
    *
    * @param interpreterGroup contains functions to and from Array[Byte]
    * @param updateF          Business logic to execute after
    * @tparam UI
    * @tparam UO
    * @tparam UE
    * @return
    */
  def put[UI, UO, UE](
                             path: String,
                             interpreterGroup: PutPostInterpreterGroup[UI, UO, UE],
                             updateF: (Long, UI) => F[Either[UE, UO]]
                           )(implicit F: Sync[F]): HttpRoutes[F] = {

    HttpRoutes.of[F] {
      case req@Method.PUT -> Root / path / LongVar(id)
        if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[F, F[Response[F]], F[Response[F]]] = for {
          body <- EitherT[F, F[Response[F]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[F] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x))
          }
          out <- EitherT[F, F[Response[F]], UO] {
            updateF(id, in).map(_.left.map(ce =>
              InternalServerError(interpreterGroup.errorInterpreter(ce))))
          }
        } yield {
          Ok(interpreterGroup.outInterpreter(out),
            Header("Content-Type", interpreterGroup.contentType))
        }
        result.value.flatMap(_.merge)
      }
    }
  }
}
