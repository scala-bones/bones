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
import org.http4s.dsl.io._
import org.http4s.util.CaseInsensitiveString
import org.http4s._

case class HttpInterpreter(charset: java.nio.charset.Charset = StandardCharsets.UTF_8 ) {

  import HttpInterpreter._

  case class DataTransformation[I, O, E](description: String,
                                         f: I => Either[E, O])


  /**
    * Creates an Http4s Routs for the given input.
    * @param serviceOps
    * @param searchF
    * @param createF
    * @param readF
    * @param updateF
    * @param deleteF
    * @tparam CI Create Input Type (Usually without an ID)
    * @tparam CO Create Output Type (Usually with an ID)
    * @tparam CE Create Error type.
    * @tparam RO Read Input Type ( Usually with an ID, probably the same as CO)
    * @tparam RE
    * @tparam UI
    * @tparam UO
    * @tparam UE
    * @tparam DO
    * @tparam DE
    * @return
    */
  def forService[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
      serviceOps: ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE],
      createF: CI => IO[Either[CE, CO]],
      readF: Long => IO[Either[RE, RO]],
      searchF: fs2.Stream[IO, RO],
      updateF: (Long, UI) => IO[Either[UE, UO]],
      deleteF: Long => IO[Either[DE, DO]]): HttpRoutes[IO] = {


    val updateHttpService = serviceOps.updateOperation.toList.flatMap(update => {
      val inputF = ValidatedFromCirceInterpreter.fromSchema(update.inputSchema)
      val outputF = EncodeToCirceInterpreter.fromSchema(update.successSchema)
      val errorF = EncodeToCirceInterpreter.fromSchema(update.failureSchema)

      val json = PutPostInterpreterGroup[UI,UO,UE](
        "application/json",
        bytes => ValidatedFromCirceInterpreter.fromByteArray(bytes, charset)
          .flatMap(json => inputF(json, List.empty)),
        uo => outputF(uo).spaces2.getBytes(charset),
        ue => errorF(ue).spaces2.getBytes(charset)
      )

      val bInputF = ValidatedFromBsonInterpreter.fromSchema(update.inputSchema)
      val bOutputF = EncodeToBson.fromSchema(update.successSchema)
      val bErrorF = EncodeToBson.fromSchema(update.failureSchema)

      val bson = PutPostInterpreterGroup[UI,UO,UE](
        "application/ubjson",
        byte => ValidatedFromBsonInterpreter.fromByteArray(byte)
          .flatMap(bjson => bInputF(bjson, List.empty)),
        uo => EncodeToBson.bsonResultToBytes(bOutputF(uo)),
        ue => EncodeToBson.bsonResultToBytes(bErrorF(ue))
      )


      val pInputF = ProtobufSequentialInputInterpreter.fromBytes(update.inputSchema)
      val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(update.successSchema)
      val pErrorF =  ProtobufSequentialOutputInterpreter.encodeToBytes(update.failureSchema)

      val protoBuf = PutPostInterpreterGroup[UI,UO,UE](
        "application/protobuf",
        bytes => pInputF(bytes),
        uo => pOutputF(uo),
        ue => pErrorF(ue)
      )

      put(json, updateF) :: put(bson, updateF) :: put(protoBuf, updateF) :: Nil

    })

    val readHttpService = serviceOps.readOperation.toList.flatMap(read => {
      val outputF = EncodeToCirceInterpreter.fromSchema(read.successSchemaForRead)
      val errorF = EncodeToCirceInterpreter.fromSchema(read.errorSchema)
      val json = GetInterpreterGroup[RO, RE](
        "application/json",
        ro => outputF(ro).spaces2.getBytes(charset),
        re => errorF(re).spaces2.getBytes(charset)
      )

      val bOutputF = EncodeToBson.fromSchema(read.successSchemaForRead)
      val bErrorF = EncodeToBson.fromSchema(read.errorSchema)
      val bson = GetInterpreterGroup[RO, RE](
        "application/ubjson",
        ro => EncodeToBson.bsonResultToBytes(bOutputF(ro)),
        re => EncodeToBson.bsonResultToBytes(bErrorF(re))
      )

      val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(read.successSchemaForRead)
      val pErrorF = ProtobufSequentialOutputInterpreter.encodeToBytes(read.errorSchema)
      val protoBuf = GetInterpreterGroup[RO,RE](
        contentType = "appliation/protobuf",
        pOutputF,
        pErrorF
      )

      get(json, readF) :: get(bson, readF) :: get(protoBuf, readF) :: Nil

    })

    val createHttpService = serviceOps.createOperation.toList.flatMap(create => {
      val inputF = ValidatedFromCirceInterpreter.fromSchema(create.inputSchema)
      val outputF = EncodeToCirceInterpreter.fromSchema(create.successSchema)
      val errorF = EncodeToCirceInterpreter.fromSchema(create.errorSchema)

      val json = PutPostInterpreterGroup[CI,CO,CE](
        "application/json",
        bytes => ValidatedFromCirceInterpreter.fromByteArray(bytes, charset)
          .flatMap(json => inputF(json, List.empty)),
        uo => outputF(uo).spaces2.getBytes(charset),
        ue => errorF(ue).spaces2.getBytes(charset)
      )

      val bInputF = ValidatedFromBsonInterpreter.fromSchema(create.inputSchema)
      val bOutputF = EncodeToBson.fromSchema(create.successSchema)
      val bErrorF = EncodeToBson.fromSchema(create.errorSchema)

      val bson = PutPostInterpreterGroup[CI,CO,CE](
        "application/ubjson",
        byte => ValidatedFromBsonInterpreter.fromByteArray(byte)
          .flatMap(bjson => bInputF(bjson, List.empty)),
        co => EncodeToBson.bsonResultToBytes(bOutputF(co)),
        ce => EncodeToBson.bsonResultToBytes(bErrorF(ce))
      )

      val pInputF = ProtobufSequentialInputInterpreter.fromBytes(create.inputSchema)
      val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(create.successSchema)
      val pErrorF = ProtobufSequentialOutputInterpreter.encodeToBytes(create.errorSchema)
      val protoBuf = PutPostInterpreterGroup[CI,CO,CE](
        "application/protobuf",
        pInputF,
        pOutputF,
        pErrorF
      )


      post(json, createF) :: post(bson, createF) :: post(protoBuf, createF) :: Nil

    })

    val deleteHttpService =
      serviceOps.deleteOperation.toList.flatMap(del => {
        val outputF = EncodeToCirceInterpreter.fromSchema(del.successSchema)
        val errorF = EncodeToCirceInterpreter.fromSchema(del.errorSchema)
        val json = DeleteInterpreterGroup[DO,DE](
          "application/json",
          dout => outputF(dout).spaces2.getBytes(charset),
          de => errorF(de).spaces2.getBytes(charset)
        )

        val bOutputF = EncodeToBson.fromSchema(del.successSchema)
        val bErrorF = EncodeToBson.fromSchema(del.errorSchema)
        val bson = DeleteInterpreterGroup[DO,DE](
          "application/ubjson",
          dout => EncodeToBson.bsonResultToBytes(bOutputF(dout)),
          de => EncodeToBson.bsonResultToBytes(bErrorF(de))
        )

        val pOutputF = ProtobufSequentialOutputInterpreter.encodeToBytes(del.successSchema)
        val pErrorF = ProtobufSequentialOutputInterpreter.encodeToBytes(del.errorSchema)
        val protobuf = DeleteInterpreterGroup[DO,DE](
          "application/protobuf",
          pOutputF,
          pErrorF
        )

        delete(json, deleteF) :: delete(bson, deleteF) :: delete(protobuf, deleteF) :: Nil

      })


    val contentTypes = "application/json" :: "application/ubjson" :: "application/protobuf" :: Nil
    val swagger = swaggerDoc(contentTypes, serviceOps)





    val services: List[HttpRoutes[IO]] =
      protoBuff(serviceOps) :: swagger :: createHttpService ::: readHttpService ::: updateHttpService ::: deleteHttpService

    services.foldLeft[HttpRoutes[IO]](HttpRoutes.empty)(
      (op1: HttpRoutes[IO], op2: HttpRoutes[IO]) => op1 <+> op2
    )

  }

}

object HttpInterpreter {

  case class PutPostInterpreterGroup[UI, UO, UE](
    contentType: String,
    inInterpreter: Array[Byte] => Either[NonEmptyList[ExtractionError], UI],
    outInterpreter: UO => Array[Byte],
    errorInterpreter: UE => Array[Byte]
  )

  case class GetInterpreterGroup[RO, RE](
    contentType: String,
    outInterpreter: RO => Array[Byte],
    errorInterpreter: RE => Array[Byte]
  )

  case class DeleteInterpreterGroup[DO,DE](
    contentType: String,
    outInterpreter: DO => Array[Byte],
    errorInterpreter: DE => Array[Byte]
  )

  case class SearchInterpreterGroup[SO](
    contentType: String,
    outInterpreter: SO => Array[Byte]
  )

  def extractionErrorToOut(pf: ParsingFailure): IO[Response[IO]] = {
    val errors = List(s"Could not parse json ${pf.message}")
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }
  def eeToOut(ee: NonEmptyList[ExtractionError]): IO[Response[IO]] = {
    val errors = ee.map(_.toString)
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }
  def missingIdToJson(id: Long): IO[Response[IO]] = {
    val errors = List(s"Could find entity with id  ${id}")
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }

  def swaggerDoc(contentTypes: List[String], serviceOps: ServiceOps[_,_,_,_,_,_,_,_,_,_]): HttpRoutes[IO] = {
    val openApi = CrudOasInterpreter.jsonApiForService(contentTypes,
      serviceOps
    )(new OpenAPI())
    val html = io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)

    HttpRoutes.of[IO] {
      case GET -> Root / "swagger" / serviceOps.path => Ok(html, Header("Content-Type", "text/html"))
    }
  }

  def protoBuff(serviceOps: ServiceOps[_,_,_,_,_,_,_,_,_,_]): HttpRoutes[IO] = {
    def toFile[A] = ProtoFileInterpreter.fromSchemaToProtoFile(_:BonesSchema[A])

    val createProto = serviceOps.createOperation.map(algebra => {
      s"""
          | // Create Input Message
          | ${toFile(algebra.inputSchema)}
          |
          | // Create Successful Output Message
          | ${toFile(algebra.successSchema)}
          |
          | // Create Error Output Message
          | ${toFile(algebra.errorSchema)}
          |
        """.stripMargin
    }).getOrElse("")

    val readProto = serviceOps.readOperation.map(algebra => {
      s"""
         |
         | // Read Successful Output Message
         | ${toFile(algebra.successSchemaForRead)}
         |
         | // Read Error Output Message
         | ${toFile(algebra.errorSchema)}
         |
        """.stripMargin
    }).getOrElse("")

    val updateProto = serviceOps.updateOperation.map(algebra => {
      s"""
         | // Update Input Message
         | ${toFile(algebra.inputSchema)}
         |
         | // Update Successful Output Message
         | ${toFile(algebra.successSchema)}
         |
         | // Update Error Output Message
         | ${toFile(algebra.failureSchema)}
         |
        """.stripMargin
    }).getOrElse("")

    val deleteProto = serviceOps.deleteOperation.map(algebra => {
      s"""
         | // Delete Successful Output Message
         | ${toFile(algebra.successSchema)}
         |
         | // Delete Error Output Message
         | ${toFile(algebra.errorSchema)}
         |
        """.stripMargin
    }).getOrElse("")

    val text = createProto + readProto + updateProto + deleteProto

    HttpRoutes.of[IO] {
      case GET -> Root / "proto" / serviceOps.path => Ok(text, Header("Content-Type", "text/plain"))
    }




  }

  def delete[DO,DE](
    interpreterGroup: DeleteInterpreterGroup[DO,DE],
    deleteF: Long => IO[Either[DE, DO]]
  ): HttpRoutes[IO] =     HttpRoutes.of[IO] {
    case req @ Method.DELETE -> Root / rootDir / LongVar(id) => {
      val result = for {
        entity <- EitherT[IO, IO[Response[IO]], DO] {
          deleteF(id).map(_.left.map(de =>
            InternalServerError(interpreterGroup.errorInterpreter(de), Header("Content-Type", interpreterGroup.contentType))))
        }
      } yield Ok(interpreterGroup.outInterpreter(entity), Header("Content-Type", interpreterGroup.contentType))

      result.value.flatMap(_.merge)
    }
  }

  def contentType(req: Request[IO]): Option[String] =
    req.headers.find(header => header.name == CaseInsensitiveString("Content-Type")).map(_.value)

  def search[CO](
    interpreterGroup: SearchInterpreterGroup[CO],
    searchF: fs2.Stream[IO,CO]
  ): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req @ Method.GET -> Root / entityRootDir if contentType(req).contains(interpreterGroup.contentType) => {
        Ok(
          Stream("[") ++ searchF.map(e => interpreterGroup.outInterpreter(e).asJson.noSpaces).intersperse(",") ++ Stream("]"),
          Header("Content-Type", "application/json")
        )
      }
    }
  }

  def post[CI, CO, CE](
                        interpreterGroup: PutPostInterpreterGroup[CI, CO, CE],
                        createF: CI => IO[Either[CE, CO]]
  ): HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case req @ Method.POST -> Root / entityRootDir if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
          body <- EitherT[IO, IO[Response[IO]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[IO] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x))
          }
          out <- EitherT[IO, IO[Response[IO]], CO] {
            createF(in)
              .map(_.left.map(ce => {
                val out = interpreterGroup.errorInterpreter(ce)
                InternalServerError(out, Header("Content-Type", interpreterGroup.contentType))
              }))
          }
        } yield {
          Ok(interpreterGroup.outInterpreter(out), Header("Content-Type", interpreterGroup.contentType))
        }
        result.value.flatMap(_.merge)
      }
    }



  /**
    * Create a get endpoint.
    * @param interpreterGroup
    * @param readF
    * @tparam RO
    * @tparam RE
    * @return
    */
  def get[RO, RE](interpreterGroup: GetInterpreterGroup[RO,RE], readF: Long => IO[Either[RE,RO]]): HttpRoutes[IO] = {
    HttpRoutes.of[IO] {
      case req @ Method.GET -> Root / rootDir / LongVar(id) if contentType(req).contains(interpreterGroup.contentType) => {
        readF(id)
          .flatMap({
            case Left(re)  => BadRequest(interpreterGroup.errorInterpreter(re), Header("Content-Type", interpreterGroup.contentType))
            case Right(ro) => Ok(interpreterGroup.outInterpreter(ro), Header("Content-Type", interpreterGroup.contentType))
          })
      }
    }
  }


  /**
    * Create a PUT endpoint given serialization functors and business logic.
    * @param interpreterGroup contains functions to and from Array[Byte]
    * @param updateF Business logic to execute after
    * @tparam UI
    * @tparam UO
    * @tparam UE
    * @return
    */
  def put[UI, UO, UE](
      interpreterGroup: PutPostInterpreterGroup[UI, UO, UE],
      updateF: (Long, UI) => IO[Either[UE, UO]]
  ): HttpRoutes[IO] = {

    HttpRoutes.of[IO] {
      case req @ Method.PUT -> Root / rootDir / LongVar(id) if contentType(req).contains(interpreterGroup.contentType) => {
        val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
          body <- EitherT[IO, IO[Response[IO]], Array[Byte]] {
            req.as[Array[Byte]].map(Right(_))
          }
          in <- EitherT.fromEither[IO] {
            interpreterGroup.inInterpreter(body).left.map(x => eeToOut(x))
          }
          out <- EitherT[IO, IO[Response[IO]], UO] {
            updateF(id, in).map(_.left.map(ce =>
              InternalServerError(interpreterGroup.errorInterpreter(ce))))
          }
        } yield {
          Ok(interpreterGroup.outInterpreter(out), Header("Content-Type", interpreterGroup.contentType))
        }
        result.value.flatMap(_.merge)
      }
    }
  }
}
