package com.bones.http4s

import cats.effect.SyncIO
import cats.implicits._
import com.bones.http4s.CrudInterpreterDescription.{GetInterpreterGroup, PutPostInterpreterGroup}
import com.bones.syntax._
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
  case class Output(result: Int)
  case class Error(message: String)

  val endpointOneSchema = (("oneName", string) :: kvpNil).convert[EndpointOne]
  val endpointTwoSchema = (("twoName", string) :: kvpNil).convert[EndpointTwo]
  val endpointThreeSchema = (("threeName", string) :: kvpNil).convert[EndpointThree]
  val outputSchema = (("result", int) :: kvpNil).convert[Output]
  val error = (("message", string) :: kvpNil).convert[Error]


  test("path matches appropriately when there are multiple get endpoints") {
    val groupOne = GetInterpreterGroup(
      "application/test",
      (o: Output) => s"Output 1: ${o.result}".getBytes,
      (err: Error) => s"Error: ${err.message}".getBytes
    )
    val groupTwo = GetInterpreterGroup(
      "application/test",
      (o: Output) => s"Output 2: ${o.result}".getBytes,
      (err: Error) => s"Error: ${err.message}".getBytes
    )

    val fOne = (id: Int) => SyncIO { Right(Output(id)) }
    val fTwo = (id: Int) => SyncIO { Right(Output(id)) }

    val routeOneDef = BaseCrudInterpreter.get[SyncIO, Error, Output, Int](
      "route-one",
      groupOne,
      BaseCrudInterpreter.intParam,
      fOne
    )

    val routeTwoDef = BaseCrudInterpreter.get[SyncIO, Error, Output, Int](
      "route-two",
      groupTwo,
      BaseCrudInterpreter.intParam,
      fTwo
    )

    val routeOneResponse = routeOneDef.orNotFound.run(
      Request(method = Method.GET, uri = uri"/route-one/7", headers = Headers.of(Header("Content-Type", "application/test")) )
    ).unsafeRunSync()

    routeOneResponse.status mustEqual(Status.Ok)
    routeOneResponse.as[String].unsafeRunSync() mustEqual "Output 1: 7"

    // now we add one more route


    val routeOneAndTwo = routeOneDef <+> routeTwoDef

    val routeOneAndTwoOneResponse = routeOneAndTwo.orNotFound.run(
      Request(method = Method.GET, uri = uri"/route-one/7", headers = Headers.of(Header("Content-Type", "application/test")) )
    ).unsafeRunSync()

    routeOneAndTwoOneResponse.status must be(Status.Ok)
    routeOneAndTwoOneResponse.as[String].unsafeRunSync() mustEqual "Output 1: 7"

    val routeOneAndTwoResponseTwo = routeOneAndTwo.orNotFound.run(
      Request(method = Method.GET, uri = uri"/route-two/55", headers = Headers.of(Header("Content-Type", "application/test")) )
    ).unsafeRunSync()
    routeOneAndTwoResponseTwo.status must be(Status.Ok)
    routeOneAndTwoResponseTwo.as[String].unsafeRunSync() mustEqual "Output 2: 55"
  }


  test("path matches appropriately when there are multiple post endpoints") {
    val groupOne =
      PutPostInterpreterGroup("application/test",
        _ => Right(EndpointOne("endpointOne")),
        (result: Output) => s"Output: ${result.result}".getBytes,
        (err: Error) => s"Error: ${err.message}".getBytes
      )
    val groupTwo =
      PutPostInterpreterGroup("application/test",
        _ => Right(EndpointTwo("endpointTwo")),
        (result: Output) => s"Output: ${result.result}".getBytes,
        (err: Error) => s"Error: ${err.message}".getBytes
      )


    val fOne = (_: EndpointOne) => SyncIO { Right(Output(1)) }
    val fTwo = (_: EndpointTwo) => SyncIO { Right(Output(2)) }

    val routeOneDef = BaseCrudInterpreter.post[SyncIO, EndpointOne, Error, Output, Long](
      "route-one",
      groupOne,
      fOne
    )

    val routeTwoDef = BaseCrudInterpreter.post[SyncIO, EndpointTwo, Error, Output, Long](
      "route-two",
      groupTwo,
      fTwo
    )

    val routeOneResponse = routeOneDef.orNotFound.run(
      Request(method = Method.POST, uri = uri"/route-one", headers = Headers.of(Header("Content-Type", "application/test")) )
    ).unsafeRunSync()

    routeOneResponse.status mustEqual(Status.Ok)
    s"Output: ${1}" mustEqual routeOneResponse.as[String].unsafeRunSync()


    // now we add one more route

    val routeOneAndTwoResponse = routeOneDef <+> routeTwoDef

    val route12Response1 = routeOneAndTwoResponse.orNotFound.run(
      Request(method = Method.POST, uri = uri"/route-one", headers = Headers.of(Header("Content-Type", "application/test")) )
    ).unsafeRunSync()

    route12Response1.status mustEqual(Status.Ok)
    val result12r1 = route12Response1.as[String]
    result12r1.unsafeRunSync mustBe s"Output: 1"

    val route12Response2 = routeOneAndTwoResponse.orNotFound.run(
      Request(method = Method.POST, uri = uri"/route-two", headers = Headers.of(Header("Content-Type", "application/test")) )
    ).unsafeRunSync()

    route12Response2.status mustEqual(Status.Ok)
    val result12r2 = route12Response2.as[String]
    result12r2.unsafeRunSync() mustBe s"Output: 2"

  }

}
