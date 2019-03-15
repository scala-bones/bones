package com.bones.doobie

import cats.data.{EitherT, NonEmptyList}
import cats.effect._
import com.bones.bson.{EncodeToBson, ValidatedFromBsonInterpreter}
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra._
import com.bones.data.Error.ExtractionError
import com.bones.data.Value.DataClass
import com.bones.doobie.Algebra.InterchangeFormat
import io.circe.syntax._
import io.circe.{Json, ParsingFailure}
import org.http4s.{HttpRoutes, _}
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.util.CaseInsensitiveString
import reactivemongo.bson.{BSONDocument, BSONValue}
import reactivemongo.bson.buffer.{ArrayBSONBuffer, ArrayReadableBuffer}
import fs2.Stream

import scala.scalajs.niocharset.StandardCharsets

case class HttpInterpreter(entityRootDir: String,
                           formats: List[InterchangeFormat[_]] = List.empty,
                           produceSwagger: Boolean = false,
                           charset: java.nio.charset.Charset = StandardCharsets.UTF_8 ) {

  import HttpInterpreter._

  def withContentTypes(format: InterchangeFormat[_]*) =
    copy(formats = format.toList ::: formats)
  def withContentType(format: InterchangeFormat[_]) =
    copy(formats = format :: formats)
  def withSwagger() = copy(produceSwagger = true)

  case class DataTransformation[I, O, E](description: String,
                                         f: I => Either[E, O])


  /**
    *
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
      val inputF = ValidatedFromCirceInterpreter.dataClass(update.inputSchema)
      val outputF = EncodeToCirceInterpreter.dataClass(update.successSchema)
      val errorF = EncodeToCirceInterpreter.dataClass(update.failureSchema)

      val json = PutPostInterpreterGroup[UI,UO,UE](
        "application/json",
        bytes => ValidatedFromCirceInterpreter.fromByteArray(bytes, charset)
          .flatMap(json => inputF(Some(json), Vector.empty)),
        uo => outputF(uo).spaces2.getBytes(charset),
        ue => errorF(ue).spaces2.getBytes(charset)
      )

      val bInputF = ValidatedFromBsonInterpreter.dataClass(update.inputSchema)
      val bOutputF = EncodeToBson.dataClass(update.successSchema)
      val bErrorF = EncodeToBson.dataClass(update.failureSchema)

      val bson = PutPostInterpreterGroup[UI,UO,UE](
        "application/ubjson",
        byte => ValidatedFromBsonInterpreter.fromByteArray(byte)
          .flatMap(bjson => bInputF(Some(bjson), Vector.empty)),
        uo => EncodeToBson.bsonResultToBytes(bOutputF(uo)),
        ue => EncodeToBson.bsonResultToBytes(bErrorF(ue))
      )

      put(json, updateF) :: put(bson, updateF) :: Nil

    })

    val createHttpService = serviceOps.createOperation.map(postJson(_, createF))
    val readHttpService = serviceOps.readOperation.map(getJson(_, readF))
    val deleteHttpService =
      serviceOps.deleteOperation.map(deleteJson(_, deleteF))

    val createBsonHttpService = serviceOps.createOperation.map(postBson(_,createF))

    val services: List[HttpRoutes[IO]] =
      createHttpService.toList ::: readHttpService.toList ::: updateHttpService ::: deleteHttpService.toList ::: createBsonHttpService.toList

    import cats.data.Kleisli
    import cats.effect._
    import cats.implicits._
    import org.http4s._
    import org.http4s.dsl.io._
    import org.http4s.implicits._
    services.foldLeft[HttpRoutes[IO]](HttpRoutes.empty)(
      (op1: HttpRoutes[IO], op2: HttpRoutes[IO]) => op1 <+> op2)

  }

}

object HttpInterpreter {

  case class PutPostInterpreterGroup[UI, UO, UE](
    contentType: String,
    inInterpreter: Array[Byte] => Either[NonEmptyList[ExtractionError], UI],
    outInterpreter: UO => Array[Byte],
    errorInterpreter: UE => Array[Byte]
  )


  def extractionErrorToOut(pf: ParsingFailure): IO[Response[IO]] = {
    val errors = Vector(s"Could not parse json ${pf.message}")
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }
  def eeToOut(ee: NonEmptyList[ExtractionError]): IO[Response[IO]] = {
    val errors = ee.map(_.toString)
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }
  def missingIdToJson(id: Long): IO[Response[IO]] = {
    val errors = Vector(s"Could find entity with id  ${id}")
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }

  def deleteJson[DO, DE](
      del: Delete[DO, DE],
      deleteF: Long => IO[Either[DE, DO]]): HttpRoutes[IO] = {
    val outInterpreter = EncodeToCirceInterpreter.dataClass(del.successSchema)
    val errorInterpreter = EncodeToCirceInterpreter.dataClass(del.errorSchema)

    HttpRoutes.of[IO] {
      case req @ Method.DELETE -> Root / rootDir / LongVar(id) => {
        val result = for {
          entity <- EitherT[IO, IO[Response[IO]], DO] {
            deleteF(id).map(_.left.map(de =>
              InternalServerError(errorInterpreter(de))))
          }
        } yield Ok(outInterpreter(entity))

        result.value.flatMap(_.merge)
      }
    }

  }

  def contentToCirce(req: Request[IO]): Either[NonEmptyList[ExtractionError], Json] = ???
  def contentToBson(req: Request[IO]): Either[NonEmptyList[ExtractionError], BSONValue] = ???

  def contentType(req: Request[IO]): Option[String] =
    req.headers.find(header => header.name == CaseInsensitiveString("Content-Type")).map(_.value)

  def bsonResultToBytes(bsonValue: BSONValue) : Array[Byte] = {
    val buffer = new ArrayBSONBuffer()
    BSONDocument.write(bsonValue.asInstanceOf[BSONDocument], buffer)
    buffer.array
  }

  def searchJson[CO](
    search: Search[CO],
    searchF: fs2.Stream[IO,CO]
  ) : HttpRoutes[IO] = {
    val outInterpreter = EncodeToCirceInterpreter.dataClass(search.successSchema)
    HttpRoutes.of[IO] {
      case req @ Method.GET -> Root / entityRootDir if contentType(req).contains("application/json") => {
        Ok(
          Stream("[") ++ searchF.map(e => outInterpreter(e).asJson.noSpaces).intersperse(",") ++ Stream("]"),
          Header("Content-Type", "application/json")
        )
      }
    }

  }

  def postBson[CI, CO, CE](
    create: Create[CI,CO,CE],
    createF: CI => IO[Either[CE,CO]]): HttpRoutes[IO] = {

    val inInterpreter = ValidatedFromBsonInterpreter.dataClass(create.schemaForCreate)
    val outInterpreter = EncodeToBson.dataClass(create.successSchemaForCreate)
    val errorOutInterpreter = EncodeToBson.dataClass(create.errorSchemaForCreate)
    HttpRoutes.of[IO] {
      case req @ Method.POST -> Root / entityRootDir if contentType(req).contains("application/ubjson") => {
        val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
          body <- EitherT[IO, IO[Response[IO]], BSONDocument] {
            req.as[Array[Byte]]
              .map(ArrayReadableBuffer(_))
              .map(BSONDocument.read)
              .map(Right(_))
          }
          in <- EitherT.fromEither[IO] {
            inInterpreter.apply(Some(body), Vector.empty).left.map(x => eeToOut(x))
          }
          out <- EitherT[IO, IO[Response[IO]], CO] {
            createF
              .apply(in)
              .map(_.left.map(ce => {
                val out = errorOutInterpreter(ce)
                InternalServerError(bsonResultToBytes(out), Header("Content-Type", "application/ubjson"))
              }))
          }
        } yield {
          Ok(bsonResultToBytes(outInterpreter(out)), Header("Content-Type", "application/ubjson"))
        }

        result.value.flatMap(_.merge)
      }
    }

  }


  def postJson[CI, CO, CE](
      create: Create[CI, CO, CE],
      createF: CI => IO[Either[CE, CO]]): HttpRoutes[IO] = {
    val inInterpreter =
      ValidatedFromCirceInterpreter.dataClass(create.schemaForCreate)
    val outInterpreter =
      EncodeToCirceInterpreter.dataClass(create.successSchemaForCreate)
    val errorOutInterpreter =
      EncodeToCirceInterpreter.dataClass(create.errorSchemaForCreate)
    HttpRoutes.of[IO] {
      case req @ Method.POST -> Root / entityRootDir if contentType(req).contains("application/json") => {

        val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
          body <- EitherT[IO, IO[Response[IO]], String] {
            req.as[String].map(Right(_))
          }
          circe <- EitherT.fromEither[IO] {
            io.circe.parser.parse(body).left.map(x => extractionErrorToOut(x))
          }
          in <- EitherT.fromEither[IO] {
            inInterpreter.apply(Some(circe), Vector.empty).left.map(x => eeToOut(x))
          }
          out <- EitherT[IO, IO[Response[IO]], CO] {
            createF
              .apply(in)
              .map(_.left.map(ce =>
                InternalServerError(errorOutInterpreter(ce))))
          }
        } yield {
          Ok(outInterpreter(out))
        }

        result.value.flatMap(_.merge)

      }
    }

  }

  //    def search(read: Read[RO,RE]): HttpService[IO] = {
  //      val interpreter = EncodeToCirceInterpreter.dataClass(read.successSchemaForRead)
  //      val errorInterpreter = EncodeToCirceInterpreter.dataClass(read.successSchemaForRead)
  //      HttpService[IO] {
  //        case Method.GET -> Root / rootDir => {
  //          val stream = dao.findAll.transact(transactor)
  //          Ok(Stream("[") ++
  //            stream.map(a => interpreter(a).noSpaces).intersperse(",") ++
  //            Stream("]"),
  //            `Content-Type`(MediaType.application.json)
  //          )
  //        }
  //      }
  //    }

  def getBson[RO,RE](read: Read[RO, RE],
                     readF: Long => IO[Either[RE, RO]]): HttpRoutes[IO] = {

    val successOut =
      EncodeToBson.dataClass(read.successSchemaForRead)
    val errorOut = EncodeToBson.dataClass(read.errorSchema)

    HttpRoutes.of[IO] {
      case req @ Method.GET -> Root / rootDir / LongVar(id) if contentType(req).contains("application/ubjson")=> {
        readF(id)
          .flatMap({
            case Left(re)  => BadRequest(bsonResultToBytes(errorOut(re)), Header("Content-Type", "application/ubjson"))
            case Right(ro) => Ok(bsonResultToBytes(successOut(ro)), Header("Content-Type", "application/ubjson"))
          })
      }
    }
  }

  def getJson[RO, RE](read: Read[RO, RE],
                      readF: Long => IO[Either[RE, RO]]): HttpRoutes[IO] = {

    val successOut =
      EncodeToCirceInterpreter.dataClass(read.successSchemaForRead)
    val errorOut = EncodeToCirceInterpreter.dataClass(read.errorSchema)

    HttpRoutes.of[IO] {
      case req @ Method.GET -> Root / rootDir / LongVar(id) if contentType(req).contains("application/json") => {
        readF(id)
          .flatMap({
            case Left(re)  => BadRequest(errorOut(re))
            case Right(ro) => Ok(successOut(ro))
          })
      }
    }
  }


  /**
    * Create a PUT endpoint given serialization functors and business logic.
    * @param interpreterGroup
    * @param updateF
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
