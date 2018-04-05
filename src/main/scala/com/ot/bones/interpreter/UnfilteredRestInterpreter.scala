package com.ot.bones.interpreter

import java.net.URLDecoder
import java.util.UUID

import com.ot.bones.data.Algebra.DataDefinitionOp
import com.ot.bones.interpreter.EncoderInterpreter.{DefaultEncoderInterpreter, Encode}
import com.ot.bones.interpreter.Processors.IdentityProcessor
import com.ot.bones.interpreter.UnfilteredRestInterpreter.{ConfigContext, Configuration}
import com.ot.bones.rest.Algebra._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import net.liftweb.json.{JValue, compact, pretty}
import unfiltered.request.HttpRequest
import unfiltered.response.{BadRequest, NotFound, Ok, ResponseFunction, ResponseString, Status}
import net.liftweb.json.JsonDSL._
import net.liftweb.json._


object UnfilteredRestInterpreter {

  type RequestResponse = ((HttpRequest[HttpServletRequest], Map[String,String]) => Option[ResponseFunction[HttpServletResponse]])
  type ServletDefinition = (String, RequestResponse)

  case class EntityNotFound[A](uuid: UUID, clazz: Class[A])
  trait EntityLoader {
    def loadById[A: Manifest](uuid: UUID): Either[EntityNotFound[A], A] = ???
  }

  trait Config {
    def instance[T:Manifest]: T = {
      val entityLoader: EntityLoader = new EntityLoader {
        override def loadById[A: Manifest](uuid: UUID): Either[EntityNotFound[A], A] = super.loadById(uuid)
      }
      val ClassOfEntityLoad = classOf[EntityLoader]
      val clazz = implicitly[Manifest[T]].runtimeClass

      clazz match {
        case ClassOfEntityLoad => entityLoader.asInstanceOf[T]
      }
    }

    def close() = {}
  }

  type Run[A,E] = Config => Either[E, A]

  class ConfigContext {
    def run[A](f: Config => A): A = {
      //load.
      val config = new Config {}
      val result = f(config)
      config.close()
      result
    }

  }

  case class Configuration[I,A,E](f: I => Run[E,A])


  def uuid(str: String): Either[ResponseFunction[HttpServletResponse],UUID] =
    try {
      Right(UUID.fromString(str))
    } catch {
      case e: IllegalArgumentException => Left(BadRequest ~> ResponseString("Invalid UUID: " + str))
    }

  def getParam[B](key: String, param: Map[String, String]): Either[ResponseFunction[HttpServletResponse], String] =
    param.get(key).map(URLDecoder.decode(_, "UTF-8")).toRight(
      BadRequest ~> ResponseString("Missing parameter:" + key)
    )

  def getUuidParam(key: String, param: Map[String, String]): Either[ResponseFunction[HttpServletResponse],UUID] =
    getParam(key, param).right.flatMap(uuid)


}

object Processors {
  type ProcessorF[I,E,R] = I => Either[E,R]

  case class IdentityProcessor[I]() extends Processor[I,Nothing,I]
}
case class Processors(configContext: ConfigContext) {



  def process[I,E,R](p: Processor[I,E,R]): Processors.ProcessorF[I,E,R] = {
    p match {
      case Processors.IdentityProcessor() => (i: I) => Right(i).asInstanceOf[Right[Nothing,R]]
      case _ => ???
    }
  }

}

case class UnfilteredRestInterpreter(
  configContext: ConfigContext,
  encoder: cats.arrow.FunctionK[DataDefinitionOp, Encode]) {

  import UnfilteredRestInterpreter._

  def apply[A:Manifest,B](restOp: RestOp[A]): List[ServletDefinition] = restOp match {
    case EndPoint(url, actions) => actions.map(a => resolveAction(a, url))
  }

//  def loadToProcessing[A,E,F](load: Load[I,A,E,F]): Processing = (request, params) => {
//    val result = for {
//      uuid <- getParam[HttpServletResponse]("uuid", params)
//      entity <- load.f(uuid)
//    }
//  }

  def render[B](status: Status, obj: JValue, req: HttpRequest[_]): ResponseFunction[HttpServletResponse] = {
    val doc = net.liftweb.json.render(obj)

    val str = if (req.parameterValues("render").contains("pretty")) pretty(doc) else compact(doc)
    status ~> ResponseString(str)
  }

  def notFound[A: Manifest](uuid: UUID, request: HttpRequest[_]) : ResponseFunction[HttpServletResponse] = {
    val response = ("error" -> ("uuid" -> uuid.toString))
    render(NotFound, response, request)
  }



  def resolveAction[A:Manifest,E,B](restOp: RestOp[A], url: String) : ServletDefinition = {
    restOp match {
      case Get(successSchema) => ???
//        (url + "/:uuid", toGetRequestResponse[A,E](successSchema))
      case Post(inputSchema, processor, successSchema, errorSchema) =>
        (url, toPostRequestResponse(inputSchema, processor, successSchema, errorSchema))
    }
  }

  private def toPostRequestResponse[I,E,R](
    inputSchema: DataDefinitionOp[I],
    processor: Processor[I,E,R],
    successSchema: DataDefinitionOp[R],
    errorSchema: DataDefinitionOp[E]
  ): RequestResponse = {
    (request: HttpRequest[HttpServletRequest], params: Map[String, String]) => {
      request.method match {
        case "POST" => configContext.run(config => ???)
      }
    }
  }

  private def toGetRequestResponse[A:Manifest,E](successSchema: DataDefinitionOp[A]): RequestResponse =
    {
      (request: HttpRequest[HttpServletRequest], params: Map[String, String]) => {
        request.method match {
          case "GET" => configContext.run(config => {
            val result = for {
              uuid <- getUuidParam("uuid", params).right
              entity <- config.instance[EntityLoader].loadById[A](uuid)
                .left.map(_ => notFound(uuid, request)).right
            } yield {
              ???
//              val toJson = successSchema.lift.foldMap[Encode](DefaultEncoderInterpreter())
//              render(Ok, toJson.apply(entity), request)
            }
            Some(result.fold(identity, identity))
          })
          case _ => None
        }
      }
    }

}
