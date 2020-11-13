package com.bones.akkahttp.server

import akka.http.scaladsl.Http
import com.bones.data.values.DefaultValues
import com.bones.syntax._
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.server.Directives._
import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Try
object ClassicCrudInterpreterExample extends App {

  implicit val system = ActorSystem(Behaviors.empty, "my-system")
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.executionContext

  case class Customer(name: String, age: Int, weight: Option[BigDecimal])

  val customerSchemaBase =
    ("name", string(sv.words)) ::
      ("age", int(iv.between(0, 200))) ::
      ("weight", bigDecimal(bdv.between(BigDecimal(0), BigDecimal(5000))).optional) :<:
      kvpNil[String]
  val customerSchema = customerSchemaBase.convert[Customer]

  case class Error(errorMessage: String)
  val errorSchema =
    (("errorMessage", string()) :: kvpNil[String]).convert[Error]

  val stringToId: String => Either[String, Long] = str =>
    Try(str.toLong).toEither.left.map(_.getMessage)

  val interpreter = ClassicCrudDefinition.empty[DefaultValues, Long, Customer, Error](
    config.defaultLongConfig,
    "customer",
    customerSchema,
    stringToId,
    errorSchema)

  val db = scala.collection.mutable.HashMap[Long, Customer]()
  var id = 1L

  val route1 = interpreter.routeForCreate(customer =>
    Future {
      val thisId = id
      db.update(thisId, customer)
      id = id + 1
      Right(thisId)
  })

  val route2 =
    interpreter.routeForGet(id =>
      Future(db.get(id).map(c => (id, c)).toRight(Error(s"No entity with id: ${id}"))))

  val bindingFuture = Http().newServerAt("localhost", 8080).bind(concat(route1, route2))

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return
  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done

}
