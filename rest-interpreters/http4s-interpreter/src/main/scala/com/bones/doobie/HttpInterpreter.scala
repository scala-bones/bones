package com.bones.doobie

import cats.data.{EitherT, NonEmptyList}
import cats.effect._
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra._
import com.bones.data.Error.ExtractionError
import com.bones.doobie.Algebra.InterchangeFormat
import com.bones.oas3.CrudOasInterpreter
import io.circe.syntax._
import io.circe.{Json, ParsingFailure}
import io.swagger.v3.oas.models.OpenAPI
import org.http4s.{HttpRoutes, _}
import org.http4s.circe._
import org.http4s.dsl.io._

case class HttpInterpreter(entityRootDir: String,
                           formats: List[InterchangeFormat[_]] = List.empty,
                           produceSwagger: Boolean = false) {

  import HttpInterpreter._

  def withContentTypes(format: InterchangeFormat[_]*) =
    copy(formats = format.toList ::: formats)
  def withContentType(format: InterchangeFormat[_]) =
    copy(formats = format :: formats)
  def withSwagger() = copy(produceSwagger = true)

  case class DataTransformation[I, O, E](description: String,
                                         f: I => Either[E, O])



  def forService[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
      serviceOps: ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE],
      createF: CI => IO[Either[CE, CO]],
      readF: Long => IO[Either[RE, RO]],
      updateF: (Long, UI) => IO[Either[UE, UO]],
      deleteF: Long => IO[Either[DE, DO]]): HttpRoutes[IO] = {

    val createHttpService = serviceOps.createOperation.map(postJson(_, createF))
    val readHttpService = serviceOps.readOperation.map(getJson(_, readF))
    val updateHttpService = serviceOps.updateOperation.map(putJson(_, updateF))
    val deleteHttpService =
      serviceOps.deleteOperation.map(deleteJson(_, deleteF))

    val services: List[HttpRoutes[IO]] =
      createHttpService.toList ::: readHttpService.toList ::: updateHttpService.toList ::: deleteHttpService.toList

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
      case req @ Method.POST -> Root / entityRootDir => {
        val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
          body <- EitherT[IO, IO[Response[IO]], String] {
            req.as[String].map(Right(_))
          }
          circe <- EitherT.fromEither[IO] {
            io.circe.parser.parse(body).left.map(x => extractionErrorToOut(x))
          }
          in <- EitherT.fromEither[IO] {
            inInterpreter.apply(Some(circe)).left.map(x => eeToOut(x))
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

  def getJson[RO, RE](read: Read[RO, RE],
                      readF: Long => IO[Either[RE, RO]]): HttpRoutes[IO] = {

    val successOut =
      EncodeToCirceInterpreter.dataClass(read.successSchemaForRead)
    val errorOut = EncodeToCirceInterpreter.dataClass(read.errorSchema)

    HttpRoutes.of[IO] {
      case Method.GET -> Root / rootDir / LongVar(id) => {
        readF(id)
          .flatMap({
            case Left(re)  => BadRequest(errorOut(re))
            case Right(ro) => Ok(successOut(ro))
          })
      }
    }
  }

  // Either[NonEmptyList[ExtractionError]
  //    type InInterpreter = (Request[IO], DataClass[A]) => IO[Either[IO[Response[IO]], A]]
  //    type OutInterpreter = (A, DataClass[A]) => IO[Response[IO]]
  //    type OutWithIdInterpreter = (A, Long, DataClass[A]) => IO[Response[IO]]

  def putJson[UI, UO, UE](
      update: Update[UI, UO, UE],
      updateF: (Long, UI) => IO[Either[UE, UO]]
  ): HttpRoutes[IO] = {
    val inInterpreter =
      ValidatedFromCirceInterpreter.dataClass(update.inputSchema)
    val outInterpreter =
      EncodeToCirceInterpreter.dataClass(update.successSchema)
    val errorInterpreter =
      EncodeToCirceInterpreter.dataClass(update.failureSchema)

    HttpRoutes.of[IO] {
      case req @ Method.PUT -> Root / rootDir / LongVar(id) => {
        val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
          body <- EitherT[IO, IO[Response[IO]], String] {
            req.as[String].map(Right(_))
          }
          circe <- EitherT.fromEither[IO] {
            io.circe.parser.parse(body).left.map(x => extractionErrorToOut(x))
          }
          in <- EitherT.fromEither[IO] {
            inInterpreter.apply(Some(circe)).left.map(x => eeToOut(x))
          }
          out <- EitherT[IO, IO[Response[IO]], UO] {
            updateF(id, in).map(_.left.map(ce =>
              InternalServerError(errorInterpreter(ce))))
          }
        } yield {
          Ok(outInterpreter.apply(out))
        }

        result.value.flatMap(_.merge)

      }

    }
  }
}