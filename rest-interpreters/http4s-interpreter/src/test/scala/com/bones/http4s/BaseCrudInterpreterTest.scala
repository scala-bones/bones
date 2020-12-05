package com.bones.http4s

import java.nio.charset.{Charset, StandardCharsets}

import cats.effect.SyncIO
import cats.implicits._
import com.bones.circe.values.isoCirceValidatorInterpreter
import com.bones.circe.{
  CirceValidatorFromByteArray,
  IsoCirceEncoderInterpreter,
  IsoCirceValidatorInterpreter
}
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common._
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator
import com.bones.syntax._
import fs2.Stream
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers._

class BaseCrudInterpreterTest extends AnyFunSuite {

  implicit object syncDsl extends Http4sDsl[SyncIO]

  case class EndpointOne(oneName: String)
  case class EndpointTwo(twoName: String)
  case class EndpointThree(threeName: String)
  case class Output(result: Int, tag: String)
  case class Error(message: String)

  val endpointOneSchema = (("oneName", string) :: kvpNil).convert[EndpointOne]
  val endpointTwoSchema = (("twoName", string) :: kvpNil).convert[EndpointTwo]
  val endpointThreeSchema = (("threeName", string) :: kvpNil).convert[EndpointThree]
  val outputSchema = (("result", int) :: ("tag", string) :: kvpNil).convert[Output]
  val error = (("message", string) :: kvpNil).convert[Error]

  val jsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      IsoCirceEncoderInterpreter(com.bones.circe.values.defaultEncoders)
        .generateEncoder(kvp)
        .map(_.noSpaces.getBytes(StandardCharsets.UTF_8))

    override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
      : Validator[String, DefaultValues, A, Array[Byte]] = {
      CirceValidatorFromByteArray()
        .andThen(isoCirceValidatorInterpreter.generateValidator(kvp))
    }
  }

  val contentInterpreters =
    ContentInterpreters(
      Content("application/test", jsonInterpreter),
      Set.empty[Content[String, DefaultValues, String]])

  test("path matches appropriately when there are multiple get endpoints") {
    val fOne = (id: Int) => SyncIO { Right(Output(id, "one")) }
    val fTwo = (id: Int) => SyncIO { Right(Output(id, "two")) }

    val endpointOneDef =
      HttpEndpointDef.defaultValues(contentInterpreters, endpointOneSchema, outputSchema, error)

    val routeOneDef =
      Http4sEndpoints.get[SyncIO, DefaultValues, Output, Int, Error, StringToIdError](
        "route-one",
        endpointOneDef,
        Path.intParam,
        fOne
      )

    val endpointTwoDef =
      HttpEndpointDef.defaultValues(contentInterpreters, endpointTwoSchema, outputSchema, error)

    val routeTwoDef =
      Http4sEndpoints.get[SyncIO, DefaultValues, Output, Int, Error, StringToIdError](
        "route-two",
        endpointTwoDef,
        Path.intParam,
        fTwo
      )

    val routeOneResponse = routeOneDef.orNotFound
      .run(
        Request(
          method = Method.GET,
          uri = uri"/route-one/7",
          headers = Headers.of(Header("Content-Type", "application/test")))
      )
      .unsafeRunSync()

    routeOneResponse.status mustEqual (Status.Ok)
    routeOneResponse.as[String].unsafeRunSync() mustEqual "{\"result\":7,\"tag\":\"one\"}"

    // now we add one more route

    val routeOneAndTwo = routeOneDef <+> routeTwoDef

    val routeOneAndTwoOneResponse = routeOneAndTwo.orNotFound
      .run(
        Request(
          method = Method.GET,
          uri = uri"/route-one/7",
          headers = Headers.of(Header("Content-Type", "application/test")))
      )
      .unsafeRunSync()

    routeOneAndTwoOneResponse.status must be(Status.Ok)
    routeOneAndTwoOneResponse.as[String].unsafeRunSync() mustEqual "{\"result\":7,\"tag\":\"one\"}"

    val routeOneAndTwoResponseTwo = routeOneAndTwo.orNotFound
      .run(
        Request(
          method = Method.GET,
          uri = uri"/route-two/55",
          headers = Headers.of(Header("Content-Type", "application/test")))
      )
      .unsafeRunSync()
    routeOneAndTwoResponseTwo.status must be(Status.Ok)
    routeOneAndTwoResponseTwo.as[String].unsafeRunSync() mustEqual "{\"result\":55,\"tag\":\"two\"}"
  }

  test("path matches appropriately when there are multiple post endpoints") {

    val groupOne =
      HttpEndpointDef.defaultValues(contentInterpreters, endpointOneSchema, outputSchema, error)

    val groupTwo =
      HttpEndpointDef.defaultValues(contentInterpreters, endpointTwoSchema, outputSchema, error)

    val fOne = (e: EndpointOne) => SyncIO { Right(Output(1, e.oneName)) }
    val fTwo = (e: EndpointTwo) => SyncIO { Right(Output(2, e.twoName)) }

    val routeOneDef =
      Http4sEndpoints.post[SyncIO, DefaultValues, EndpointOne, Output, Int, Error, StringToIdError](
        "route-one",
        groupOne,
        fOne
      )

    val routeTwoDef =
      Http4sEndpoints.post[SyncIO, DefaultValues, EndpointTwo, Output, Int, Error, StringToIdError](
        "route-two",
        groupTwo,
        fTwo
      )

    val routeOneResponse = routeOneDef.orNotFound
      .run(
        Request(
          method = Method.POST,
          uri = uri"/route-one",
          body =
            Stream.emits[SyncIO, Byte]("""{"oneName":"one"}""".getBytes(Charset.forName("UTF-8"))),
          headers = Headers.of(Header("Content-Type", "application/test"))
        )
      )
      .unsafeRunSync()

    routeOneResponse.status mustEqual (Status.Ok)
    s"""{"result":1,"tag":"one"}""" mustEqual routeOneResponse.as[String].unsafeRunSync()

    // now we add one more route

    val routeOneAndTwoResponse = routeOneDef <+> routeTwoDef

    val route12Response1 = routeOneAndTwoResponse.orNotFound
      .run(
        Request(
          method = Method.POST,
          uri = uri"/route-one",
          body =
            Stream.emits[SyncIO, Byte]("""{"oneName":"one"}""".getBytes(Charset.forName("UTF-8"))),
          headers = Headers.of(Header("Content-Type", "application/test"))
        )
      )
      .unsafeRunSync()

    route12Response1.status mustEqual (Status.Ok)
    val result12r1 = route12Response1.as[String]
    result12r1.unsafeRunSync mustBe s"""{"result":1,"tag":"one"}"""

    val route12Response2 = routeOneAndTwoResponse.orNotFound
      .run(
        Request(
          method = Method.POST,
          uri = uri"/route-two",
          body =
            Stream.emits[SyncIO, Byte]("""{"twoName":"two"}""".getBytes(Charset.forName("UTF-8"))),
          headers = Headers.of(Header("Content-Type", "application/test"))
        )
      )
      .unsafeRunSync()

    route12Response2.status mustEqual (Status.Ok)
    val result12r2 = route12Response2.as[String]
    result12r2.unsafeRunSync() mustBe s"""{"result":2,"tag":"two"}"""

  }

}
