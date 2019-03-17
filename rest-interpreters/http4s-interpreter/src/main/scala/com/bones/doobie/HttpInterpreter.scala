package com.bones.doobie

import cats.data.{EitherT, NonEmptyList}
import cats.effect._
import com.bones.bson.{EncodeToBson, ValidatedFromBsonInterpreter}
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra._
import com.bones.data.Error.ExtractionError
import com.bones.data.Value.DataClass
import com.bones.doobie.Algebra.InterchangeFormat
import com.bones.doobie.HttpInterpreter.DeleteInterpreterGroup
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

    val readHttpService = serviceOps.readOperation.toList.flatMap(read => {
      val outputF = EncodeToCirceInterpreter.dataClass(read.successSchemaForRead)
      val errorF = EncodeToCirceInterpreter.dataClass(read.errorSchema)
      val json = GetInterpreterGroup[RO, RE](
        "application/json",
        ro => outputF(ro).spaces2.getBytes(charset),
        re => errorF(re).spaces2.getBytes(charset)
      )

      val bOutputF = EncodeToBson.dataClass(read.successSchemaForRead)
      val bErrorF = EncodeToBson.dataClass(read.errorSchema)
      val bson = GetInterpreterGroup[RO, RE](
        "application/ubjson",
        ro => EncodeToBson.bsonResultToBytes(bOutputF(ro)),
        re => EncodeToBson.bsonResultToBytes(bErrorF(re))
      )

      get(json, readF) :: get(bson, readF) :: Nil

    })

    val createHttpService = serviceOps.createOperation.toList.flatMap(create => {
      val inputF = ValidatedFromCirceInterpreter.dataClass(create.inputSchema)
      val outputF = EncodeToCirceInterpreter.dataClass(create.successSchema)
      val errorF = EncodeToCirceInterpreter.dataClass(create.errorSchema)

      val json = PutPostInterpreterGroup[CI,CO,CE](
        "application/json",
        bytes => ValidatedFromCirceInterpreter.fromByteArray(bytes, charset)
          .flatMap(json => inputF(Some(json), Vector.empty)),
        uo => outputF(uo).spaces2.getBytes(charset),
        ue => errorF(ue).spaces2.getBytes(charset)
      )

      val bInputF = ValidatedFromBsonInterpreter.dataClass(create.inputSchema)
      val bOutputF = EncodeToBson.dataClass(create.successSchema)
      val bErrorF = EncodeToBson.dataClass(create.errorSchema)

      val bson = PutPostInterpreterGroup[CI,CO,CE](
        "application/ubjson",
        byte => ValidatedFromBsonInterpreter.fromByteArray(byte)
          .flatMap(bjson => bInputF(Some(bjson), Vector.empty)),
        co => EncodeToBson.bsonResultToBytes(bOutputF(co)),
        ce => EncodeToBson.bsonResultToBytes(bErrorF(ce))
      )

      post(json, createF) :: post(bson, createF) :: Nil

    })

    val deleteHttpService =
      serviceOps.deleteOperation.toList.flatMap(del => {
        val outputF = EncodeToCirceInterpreter.dataClass(del.successSchema)
        val errorF = EncodeToCirceInterpreter.dataClass(del.errorSchema)
        val json = DeleteInterpreterGroup[DO,DE](
          "application/json",
          dout => outputF(dout).spaces2.getBytes(charset),
          de => errorF(de).spaces2.getBytes(charset)
        )

        val bOutputF = EncodeToBson.dataClass(del.successSchema)
        val bErrorF = EncodeToBson.dataClass(del.errorSchema)
        val bson = DeleteInterpreterGroup[DO,DE](
          "application/json",
          dout => EncodeToBson.bsonResultToBytes(bOutputF(dout)),
          de => EncodeToBson.bsonResultToBytes(bErrorF(de))
        )

        delete(json, deleteF) :: delete(bson, deleteF) :: Nil

      })


    val services: List[HttpRoutes[IO]] =
      createHttpService ::: readHttpService ::: updateHttpService ::: deleteHttpService

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
