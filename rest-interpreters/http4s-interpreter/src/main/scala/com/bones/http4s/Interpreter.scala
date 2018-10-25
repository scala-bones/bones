package com.bones.http4s

import cats.FlatMap
import cats.data.{EitherT, NonEmptyList}
import cats.effect._
import cats.implicits._
import org.http4s.implicits._
import com.bones.circe.{EncodeToCirceInterpreter, ValidatedFromCirceInterpreter}
import com.bones.crud.Algebra._
import com.bones.data.Error.ExtractionError
import com.bones.http4s.Algebra.InterchangeFormat
import com.bones.http4s.Orm.Dao
import doobie.implicits._
import doobie.util.transactor.Transactor
import fs2.Stream
import io.circe.{Json, JsonObject, ParsingFailure}
import io.circe.syntax._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import org.http4s.headers.`Content-Type`

object Interpreter {

  case class Http4s(rootDir: String, formats: List[InterchangeFormat[_]], produceSwagger: Boolean) {
    def contentTypes(format: InterchangeFormat[_]*) = copy(formats = format.toList ::: formats)
    def contentType(format: InterchangeFormat[_]) = copy(formats = format :: formats)
    def withSwagger() = copy(produceSwagger = true)
  }

  def http4s(rootDir: String) = Http4s(rootDir, List.empty, false)


  def saveWithDoobieInterpreter[A](servletDefinitions: List[CrudOp[A]], dao: Dao.Aux[A, Int], transactor: Transactor.Aux[IO,Unit]) : HttpService[IO] = {

    def deleteJson(del: Delete[A]): HttpService[IO] = {
      val outInterpreter = EncodeToCirceInterpreter().apply(del.successSchema)

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
      val inInterpreter = ValidatedFromCirceInterpreter().apply(create.schemaForCreate)
      val outInterpreter = EncodeToCirceInterpreter().apply(create.successSchemaForCreate)
      val errorInterpreter = EncodeToCirceInterpreter().apply(create.errorSchemaForCreate)
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
      val interpreter = EncodeToCirceInterpreter().apply(read.successSchemaForRead)
      HttpService[IO] {
        case Method.GET -> Root / rootDir => {
          val stream = dao.findAll.transact(transactor)
          Ok(Stream("[") ++
            stream.map(a => interpreter.apply(a).noSpaces).intersperse(",") ++
            Stream("]"),
            `Content-Type`(MediaType.`application/json`)
          )
        }
      }
    }

    def getJson[IN](read: Read[A]): HttpService[IO] = {
      val interpreter = EncodeToCirceInterpreter().apply(read.successSchemaForRead)
      val errorJson = Json.obj(("success" -> Json.fromString("false")), ("error" -> Json.fromString("could not find it")))

      def toResult(opt: Option[A], id: Int): IO[Response[IO]] = {
        opt match {
          case Some(a) => Ok(Json.obj(("id", Json.fromInt(id)) :: interpreter.apply(a).asObject.map(_.toList).getOrElse(List.empty): _*))
          case None => NotFound(errorJson)
        }
      }

      HttpService[IO] {
        case Method.GET -> Root / rootDir / IntVar(id) => {
          val prog: IO[Option[A]] = for {
            a <- dao.find(id).transact(transactor)
          } yield {
            a
          }
          prog.flatMap(toResult(_, id))
        }
      }
    }

    def putJson2[E](update: Update[A,E,A]): HttpService[IO] = {
      val inInterpreter = ValidatedFromCirceInterpreter().apply(update.inputSchema)
      val outInterpreter = EncodeToCirceInterpreter().apply(update.successSchema)
      val errorInterpreter = EncodeToCirceInterpreter().apply(update.failureSchema)
      HttpService[IO] {
        case req@Method.PUT -> Root / rootDir / IntVar(id) => {
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
            _ <- EitherT[IO, Nothing, Int] {
              dao.update(id, in).transact(transactor).map(r => Right(r))
            }
            a <- EitherT[IO, IO[Response[IO]], A] {
              dao.find(id).map(_.toRight(missingIdToJson(id))).transact(transactor)
            }
          } yield {
            Ok(outInterpreter.apply(a))
          }

          //This doesn't feel right.  Any help on this?
          result.merge.map(_.unsafeRunSync())

        }
      }
    }

    def putJson[E](update: Update[A,E,A]): HttpService[IO] = {
      val inInterpreter = ValidatedFromCirceInterpreter().apply(update.inputSchema)
      val outInterpreter = EncodeToCirceInterpreter().apply(update.successSchema)
      val errorInterpreter = EncodeToCirceInterpreter().apply(update.failureSchema)
      HttpService[IO] {
        case req@Method.PUT -> Root / rootDir / IntVar(id) => {
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
            _ <- EitherT[IO, Nothing, Int] {
              dao.update(id, in).transact(transactor).map(r => Right(r))
            }
            a <- EitherT[IO, IO[Response[IO]], A] {
              dao.find(id).map(_.toRight(missingIdToJson(id))).transact(transactor)
            }
          } yield {
            Ok(outInterpreter.apply(a))
          }

          //This doesn't feel right.  Any help on this?
          result.merge.map(_.unsafeRunSync())

        }
      }
    }

    val services = servletDefinitions.flatMap {
      case read: Read[A] => List(getJson(read), search(read))
      case delete: Delete[A] => List(deleteJson(delete))
      case create: Create[A,e,A] => List(postJson(create))
      case update: Update[A,e,A] => List(putJson(update))
      case _ => None
    }
    import cats.effect._, org.http4s._, org.http4s.dsl.io._, scala.concurrent.ExecutionContext.Implicits.global
    import cats.implicits._
    import org.http4s.server.blaze._
    import org.http4s.implicits._
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
