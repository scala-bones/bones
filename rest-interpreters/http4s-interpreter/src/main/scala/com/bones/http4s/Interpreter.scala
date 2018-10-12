package com.bones.http4s

import cats.effect._
import cats.implicits._
import com.bones.crud.Algebra.{CrudOp, Read, Update}
import com.bones.http4s.Algebra.InterchangeFormat
import com.bones.http4s.Orm.Dao
import com.bones.interpreter.{EncodeToJValueInterpreter, ValidatedFromJObjectInterpreter}
import doobie.free.connection.ConnectionIO
import doobie.util.transactor.Transactor
import doobie.implicits._
import org.http4s.HttpService
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._

object Interpreter {

  case class Http4s(rootDir: String, formats: List[InterchangeFormat[_]], produceSwagger: Boolean) {
    def contentTypes(format: InterchangeFormat[_]*) = copy(formats = format.toList ::: formats)
    def contentType(format: InterchangeFormat[_]) = copy(formats = format :: formats)
    def withSwagger() = copy(produceSwagger = true)
  }

  def http4s(rootDir: String) = Http4s(rootDir, List.empty, false)


  def saveWithDoobieInterpreter[A](servletDefinitions: List[CrudOp[A]], dao: Dao.Aux[A, Int], transactor: Transactor.Aux[IO,Unit]) : HttpService[IO] = {

    def getJson[IN](read: Read[A]): HttpService[IO] = {
      import net.liftweb.json._
      import net.liftweb.json.JsonDSL._
      val interpreter = EncodeToJValueInterpreter().apply(read.successSchemaForRead)
      val errorJson = ("success" -> "false") ~ ("error" -> "could not find it")

      def toResult(opt: Option[A], id: Int): IO[Response[IO]] = {
        opt match {
          case Some(a) => Ok(prettyRender(JObject.apply(JField("id", JInt(id)) :: interpreter.apply(a).asInstanceOf[JObject].obj)))
          case None => NotFound(prettyRender(errorJson))
        }
      }

      HttpService[IO] {
        case Method.GET -> Root / rootDir / IntVar(id) => {
          val prog: IO[Option[A]] = for {
            a <- dao.find(id).transact(transactor)
          } yield {
            a
          }
          prog.flatMap(toResult(_,id))
        }
      }
    }

    def putJson[IN,E,OUT](update: Update[IN,E,OUT]): HttpService[IO] = {
      val inInterpreter = ValidatedFromJObjectInterpreter().apply(update.inputSchema)
      val outInterpreter = EncodeToJValueInterpreter().apply(update.successSchema)
      val errorInterpreter = EncodeToJValueInterpreter().apply(update.failureSchema)
      HttpService[IO] {
        case Method.PUT -> Root / rootDir / IntVar(id) => {
          val prog: IO[A] = for {
            in <- inInterpreter.apply()
            _ <- dao.update(id, )
          }
        }
      }

    }

    val services = servletDefinitions.flatMap {
      case read: Read[A] => Some(getJson(read))
      case _ => None
    }
    // .foldLeft[HttpService[IO]](HttpService.empty)( (all,s) => all.compose(s))
    services.head

  }



}
