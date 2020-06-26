package com.bones

import cats.data.NonEmptyList
import com.bones.circe.IsoCirceEncoderAndValidatorInterpreter
import org.scalatest.funsuite.AnyFunSuite


class ValidationTest extends AnyFunSuite {

  test("append obj") {

    import com.bones.syntax._

    val o1 =
      ("key1", string) ::
        ("key2", string) ::
        kvpNil


    val o2 =
      ("key3", string) ::
        ("key4", string) ::
        kvpNil

    //    val o3 = o1 append o2
  }

  test("issue") {
    //    import shapeless._
    //
    //    case class Wrap[L <: HList](wrapped: L)
    //
    //    val x = Wrap("x" :: "y" :: HNil)
    //    val y = Wrap(1 :: 2 :: HNil)
    //
    //    case class Append[L1, L2](w1: Wrap[L1], w2: Wrap[L2], prepend: Prepend[L1, L2], lengthO: Length[L1])
    //
    //    def append[L1, L2](w1: Wrap[L1], w2: Wrap[L2])(implicit prepend: Prepend[L1, L2], lengthO: Length[L1]) = Append(w1, w2, prepend, lengthO)
    //
    //    val xAppendY = append(x,y)
    //
    //    val merged = xAppendY.prepend(xAppendY.w1.wrapped, xAppendY.w2.wrapped)
    //
    //    val split = Split[xAppendY.prepend.Out, xAppendY.hLength.Out]
    //
    //    split.apply(merged)


  }

  test("append generalization") {

    import com.bones.syntax._


    val s2 =
      ("val1", string()) ::
        ("val2", string) ::
        kvpNil

    val i2 =
      ("int1", long) ::
        ("int2", long) ::
        kvpNil

    val merged = s2 ::: i2

    //    val s = implicitly[Split[result.p.Out, result.lpLength.Out]]
    //    val s = Split[String :: String :: Int :: Int, Nat._2]

    case class Out(s1: String, s2: String, i1: Int, i2: Int)

    val input =
      """
        |{
        |  "val1" : "one",
        |  "val2" : "two",
        |  "int1" : 1,
        |  "int2" : 2
        |}
      """.stripMargin
    //    val parsed = net.liftweb.json.parse(input)

    //    val extResult = merged.apply(jsonProducer)
    //
    //    assert(extResult.toOption.get === Out("one", "two", 1, 2))


  }

  test("text max prepend") {
    //    import com.bones.syntax._
    //
    //    val x = key("test").string().optional().asMember
    //
    //    val y = x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x ::
    //      x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x :: x
    //    val cc =
    //      """
    //        |{
    //        |  "test" : "12345",
    //        |  "lastFour" : "4321",
    //        |  "uuid" : "df15f08c-e6bd-11e7-aeb8-6003089f08b4",
    //        |  "token" : "e58e7dda-e6bd-11e7-b901-6003089f08b4",
    //        |  "ccType" : "mastercard"
    //        |}
    //      """.stripMargin
    //
    //    //sorry, we still use lift in my projects.  I will soon createOperation a Circe JsonExtract.
    //    val parsed = net.liftweb.json.parse(cc)
    //    val jsonProducer = LiftJsonExtract(parsed)
    //
    //    //createOperation the program that is responsible for converting JSON into a CC.
    //    val jsonToCCProgram = y.lift.foldMap[ValidateFromProducer](ValidatedFromJObjectInterpreter())
    //
    //    //here, we will test that just the validations step is working
    //    val btCc = jsonToCCProgram.apply(jsonProducer)
    //    btCc.isValid

  }

  test("error paths") {
    import com.bones.schemas.Schemas._

    //sorry, we still use lift in my projects.  I will soon createOperation a Circe JsonExtract.
    val parsed = io.circe.parser.parse(ccBadBilling).toOption.get

    //createOperation the program that is responsible for converting JSON into a CC.
    //    val jsonToCCProgram = creditCardSchema.lift.foldMap[ValidatedFromJObjectOpt](ValidatedFromJObjectInterpreter())
    val jsonToCCProgram = IsoCirceEncoderAndValidatorInterpreter.validatorFromCustomSchema(creditCardSchema, com.bones.circe.custom.allValidators)

    //here, we will test that just the validations step is working
    val btCc = jsonToCCProgram.apply(parsed)

    btCc match {
      case Left(NonEmptyList(head, tail)) => {
        assert(head.path === List("billingLocation", "countryIso"))
      }
      case Right(x) => fail(s"Expected fail, received ${x}")
    }
  }

  test("validations example") {

    //    import SwaggerSchemas._
    //
    //    //sorry, we still use lift in my projects.  I will soon createOperation a Circe JsonExtract.
    //    val parsed = net.liftweb.json.parse(cc)
    //
    //    //createOperation the program that is responsible for converting JSON into a CC.
    ////    val jsonToCCProgram = creditCardSchema.lift.foldMap[ValidatedFromJObjectOpt](ValidatedFromJObjectInterpreter())
    //    val jsonToCCProgram = ValidatedFromJObjectInterpreter().kvpHList(creditCardSchema)
    //
    //    //here, we will test that just the validations step is working
    //    val btCc = jsonToCCProgram.apply(parsed)
    //
    //    //tada!  We have can parse input from JsonExtract to CC using our dataDefinition.
    //    assert(btCc == Right(CC("12345", "4321", UUID.fromString("df15f08c-e6bd-11e7-aeb8-6003089f08b4"),
    //      UUID.fromString("e58e7dda-e6bd-11e7-b901-6003089f08b4"), CreditCardTypes.Mastercard, 11, 2022,
    //      "Lennart Augustsson", Currency.USD, None, UUID.fromString("4545d9da-e6be-11e7-86fb-6003089f08b4"),
    //      BillingLocation("US", Some("80031"))
    //    )))
    //
    //    //convert back to json
    //    val ccToBson = EncodeToJValueInterpreter()
    //    import net.liftweb.json._
    //    val output = ccToBson.value(creditCardSchema).apply(btCc.toOption.get.head)
    //    val printed = compactRender(output)
    //    assert(printed === """{"firstSix":"12345","lastFour":"4321","uuid":"df15f08c-e6bd-11e7-aeb8-6003089f08b4","token":"e58e7dda-e6bd-11e7-b901-6003089f08b4","ccType":"Mastercard","expMonth":11,"expYear":2022,"cardHolder":"Lennart Augustsson","currencyEnum":"GBP","currencyIso":"USD","lastModifiedRequest":"4545d9da-e6be-11e7-86fb-6003089f08b4","billingLocation":{"countryIso":"US","zipCode":"80031"}}""")
    //
    //
    //    val docResult = SwaggerCoreInterpreter(creditCardSchema)
    //
    //    val components = new Components()
    //    components.addSchemas("creditCard", docResult)
    //    val openApi = new OpenAPI()
    //    openApi.components(components)

    //    println(io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi))


    //    {
    //      "statusCode": 400,
    //      "error": "Bad Request",
    //      "message": "invalid query"
    //    }


  }

}
