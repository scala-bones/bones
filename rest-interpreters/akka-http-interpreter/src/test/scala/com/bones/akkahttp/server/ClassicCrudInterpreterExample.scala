package com.bones.akkahttp.server

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentType, ContentTypes}
import akka.http.scaladsl.server.Directives._
import com.bones.data.values.DefaultValues
import com.bones.data.{CommonValues, KvpCollection}
import com.bones.http.common.Path.longParam
import com.bones.http.common._
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator
import com.bones.sprayjson.SprayValidatorFromByteArray
import com.bones.syntax._

import java.nio.charset.{Charset, StandardCharsets}
import scala.concurrent.Future
import scala.io.StdIn
object ClassicCrudInterpreterExample extends App {

  implicit val system = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.executionContext

  case class Customer(name: String, age: Int, weight: Option[BigDecimal])

  val charset: Charset = StandardCharsets.UTF_8

  val jsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      com.bones.sprayjson.values.isoSprayEncoderInterpreter
        .generateEncoder(kvp)
        .map(_.compactPrint.getBytes(charset))

    override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
      : Validator[String, DefaultValues, A, Array[Byte]] = {
      val validator = com.bones.sprayjson.values.isoSprayValidatorInterpreter
        .generateValidator(kvp)
      SprayValidatorFromByteArray(charset)
        .andThen(validator)
    }
  }

  val contentTypeEncoders = ContentInterpreters[String, DefaultValues, ContentType](
    Content(ContentTypes.`application/json`, jsonInterpreter),
    Set.empty)

  case class Error(errorMessage: String)
  val errorSchema =
    (("errorMessage", string()) :: kvpNil).convert[Error]

  val customerSchemaBase =
    ("name", string(sv.words)) ::
      ("age", int(iv.between(0, 200))) ::
      ("weight", bigDecimal(bdv.between(BigDecimal(0), BigDecimal(5000))).optional) :<:
      kvpNil
  val customerSchema = customerSchemaBase.convert[Customer]

  val classicCrudDef =
    ClassicCrudDef[DefaultValues, Customer, Long, ContentType, Error, StringToIdError](
      contentTypeEncoders,
      "customer",
      customerSchema,
      longParam,
      StringToIdError.stringToIdErrorSchema,
      errorSchema,
      CommonValues.longIdDefinition,
      "id",
      core => shapeless.Inl(core)
    )
  val db = scala.collection.mutable.HashMap[Long, Customer]()
  var id = 1L

  val route1 = ClassicCrud.endpointForCreate(
    classicCrudDef,
    (customer: Customer) =>
      Future {
        val thisId = id
        db.update(thisId, customer)
        id = id + 1
        Right((thisId, customer))
    })

  val route2 =
    ClassicCrud.getEndpoint(
      classicCrudDef,
      (id: Long) =>
        Future(db.get(id).map(c => (id, c)).toRight(Error(s"No entity with id: ${id}"))))

  val bindingFuture = Http().newServerAt("localhost", 8080).bind(concat(route1, route2))

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done

}
