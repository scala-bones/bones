package com.bones.http4s

import cats.FlatMap
import cats.data.{EitherT, NonEmptyList, OptionT, Validated}
import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra._
import com.bones.data.Error.ExtractionError
import com.bones.data.Value.ValueDefinitionOp
import com.bones.http4s.Algebra.InterchangeFormat
import com.bones.http4s.Orm.Dao
import com.bones.syntax._
import doobie.implicits._
import doobie.util.transactor.Transactor
import fs2.Stream
import io.circe.{Json, JsonObject, ParsingFailure}
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import org.http4s.headers.`Content-Type`

case class HttpInterpreter(
  entityRootDir: String,
  formats: List[InterchangeFormat[_]] = List.empty,
  produceSwagger: Boolean = false) {

  def withContentTypes(format: InterchangeFormat[_]*) = copy(formats = format.toList ::: formats)
  def withContentType(format: InterchangeFormat[_]) = copy(formats = format :: formats)
  def withSwagger() = copy(produceSwagger = true)


  def saveWithDoobieInterpreter[A](servletDefinitions: List[CrudOp[A]], dao: Dao.Aux[A, Int], transactor: Transactor.Aux[IO,Unit]) : HttpService[IO] = {

    def deleteJson(del: Delete[A]): HttpService[IO] = {
      val outInterpreter = EncodeToCirceInterpreter.apply(del.successSchema)

      HttpService[IO] {
        case req@Method.DELETE -> Root / rootDir / IntVar(id) => {
          val result = for {
            entity <- EitherT[IO, IO[Response[IO]], A] {
              dao.find(id).transact(transactor).map(_.toRight(missingIdToJson(id)))
            }
            _ <- EitherT[IO, IO[Response[IO]], Int]{ dao.delete(id).map(Right(_)).transact(transactor) }
          } yield Ok(outInterpreter.apply(entity))

          result.merge.unsafeRunSync()

        }
      }

    }

    def postJson[E](create: Create[A,E,A]): HttpService[IO] = {
      val inInterpreter = ValidatedFromCirceInterpreter(create.schemaForCreate)
      val outInterpreter = EncodeToCirceInterpreter(create.successSchemaForCreate)
      HttpService[IO] {
        case req@Method.POST -> Root / rootDir => {
          val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
            body <- EitherT[IO, IO[Response[IO]], String] {
              req.as[String].map(Right(_))
            }
            circe <- EitherT.fromEither[IO] {
              io.circe.parser.parse(body).left.map(x => extractionErrorToOut(x))
            }
            in <- EitherT.fromEither[IO] {
              inInterpreter.apply(circe).toEither.left.map(x => eeToOut(x))
            }
            id <- EitherT[IO, Nothing, Int] {
              dao.insert(in).transact(transactor).map(r => Right(r))
            }
            a <- EitherT[IO, IO[Response[IO]], A] {
              dao.find(id).map(_.toRight(missingIdToJson(id))).transact(transactor)
            }
          } yield {
            Ok(Json.obj( ("id", Json.fromInt(id)) :: outInterpreter.apply(a).asObject.map(_.toList).getOrElse(List.empty) :_*))
          }

          //This doesn't feel right.  Any help on this?
          result.merge.map(_.unsafeRunSync())

        }
      }

    }

    def search(read: Read[A]): HttpService[IO] = {
      val interpreter = EncodeToCirceInterpreter(read.successSchemaForRead)
      HttpService[IO] {
        case Method.GET -> Root / rootDir => {
          val stream = dao.findAll.transact(transactor)
          Ok(Stream("[") ++
            stream.map(a => interpreter(a).noSpaces).intersperse(",") ++
            Stream("]"),
            `Content-Type`(MediaType.`application/json`)
          )
        }
      }
    }

    def getJson[IN](read: Read[A], outInterpreter: OutInterpreter[A]): HttpService[IO] =
      HttpService[IO] {
        case Method.GET -> Root / rootDir / IntVar(id) => {
          dao.find(id).transact(transactor).flatMap{
            case Some(a) => outInterpreter.apply(a, read.successSchemaForRead)
            case None => missingIdToJson(id)
          }
        }
      }

    // Either[NonEmptyList[ExtractionError]
    type InInterpreter[A] = (Request[IO], ValueDefinitionOp[A]) => IO[Either[IO[Response[IO]], A]]
    type OutInterpreter[A] = (A, ValueDefinitionOp[A]) => IO[Response[IO]]
    type OutWithIdInterpreter[A] = (A, Long, ValueDefinitionOp[A]) => IO[Response[IO]]



    def putJson[E](
                    update: Update[A,E,A],
                    inInterpreter: InInterpreter[A],
                    outInterpreter: OutInterpreter[A]
                  ): HttpService[IO] = {
      HttpService[IO] {
        case req@Method.PUT -> Root / rootDir / IntVar(id) => {
          val result: EitherT[IO, IO[Response[IO]], IO[Response[IO]]] = for {
            in <- EitherT[IO, IO[Response[IO]], A] {
              inInterpreter(req, update.inputSchema)
            }
            _ <- EitherT[IO, Nothing, Int] {
              dao.update(id, in).transact(transactor).map(r => Right(r))
            }
            a <- EitherT[IO, IO[Response[IO]], A] {
              dao.find(id).map(_.toRight(missingIdToJson(id))).transact(transactor)
            }
          } yield {
            outInterpreter(a, update.successSchema)
          }

          //This doesn't seem right.  Any help on this?
          result.merge.map(_.unsafeRunSync())

        }
      }
    }

    val jsonInInterpreter: InInterpreter[A] =
      (req: Request[IO], valueDefinitionOp: ValueDefinitionOp[A]) => {
        for {
          body <- EitherT[IO, IO[Response[IO]], String] {
            req.as[String].map(Right(_))
          }
          circe <- EitherT.fromEither[IO] {
            io.circe.parser.parse(body).left.map(x => extractionErrorToOut(x))
          }
          a <- EitherT.fromEither[IO] {
            ValidatedFromCirceInterpreter(valueDefinitionOp)(circe).toEither
              .left.map(eeToOut)
          }
        }  yield {
          a
        }
      }.value

    val jsonOutInterpreter: OutInterpreter[A] =
      (a: A, valueDefinitionOp: ValueDefinitionOp[A]) => {
        Ok(EncodeToCirceInterpreter(valueDefinitionOp)(a))
      }
//    val jsonOutWithIdInterpreter: OutWithIdInterpreter[A] =
//      (a: A, id: Long, valueDefinitionOp: ValueDefinitionOp[A]) => {
//        import com.bones.syntax._
//        import com.bones.validation.ValidationDefinition.IntValidation._
//
//        val outWithIdValueDefinition = key("id").int(positive()) :: valueDefinitionOp
//
//      }

    val services = servletDefinitions.flatMap {
      case read: Read[A] => List(getJson(read, jsonOutInterpreter), search(read))
      case delete: Delete[A] => List(deleteJson(delete))
      case create: Create[A,e,A] => List(postJson(create))
      case update: Update[A,e,A] => List(putJson(update, jsonInInterpreter, jsonOutInterpreter))
      case _ => None
    }
    import cats.effect._, org.http4s._
    services.foldLeft[HttpService[IO]](HttpService.empty)( (op1: HttpService[IO], op2: HttpService[IO]) => op1 <+> op2)


  }


  def extractionErrorToOut(pf: ParsingFailure) : IO[Response[IO]] = {
    val errors = Vector(s"Could not parse json ${pf.message}")
    BadRequest( Json.obj(("success", "false".asJson), ("errors", errors.asJson)) )
  }
  def eeToOut(ee: NonEmptyList[ExtractionError]): IO[Response[IO]] = {
    val errors = ee.map(_.toString)
    BadRequest(Json.obj(("success", "false".asJson), ("errors", errors.asJson)))
  }
  def missingIdToJson(id: Int) : IO[Response[IO]] = {
    val errors = Vector(s"Could find entity with id  ${id}")
    BadRequest( Json.obj(("success", "false".asJson), ("errors", errors.asJson)) )
  }

}
