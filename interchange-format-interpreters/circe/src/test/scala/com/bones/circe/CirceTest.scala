package com.bones.circe

import java.nio.charset.Charset

import com.bones.scalacheck.{NoAlgebraGen, Scalacheck}
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers



class CirceTest extends FunSuite with Checkers with MustMatchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val validateFromCirce = CirceValidatorInterpreter.isoInterpreter

  val noAlgebraInterpreter = CirceValidatorInterpreter.noAlgebraInterpreter

  val interchangeFormatEncoder = CirceEncoderInterpreter.noAlgebraEncoder

  val jsonToCc = validateFromCirce.byteArrayFuncFromSchema(allSupportCaseClass, Charset.forName("UTF8"), noAlgebraInterpreter )
  val ccToJson = CirceEncoderInterpreter.isoInterpreter.fromSchema(allSupportCaseClass, interchangeFormatEncoder)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass, NoAlgebraGen))
  val utf8 = Charset.forName("UTF8")


  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: AllSupported) => {
      val json = ccToJson.apply(cc)
      val jsonString = json.spaces2.getBytes(utf8)
      val newCc = jsonToCc(jsonString)
      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for JSON string ${io.circe.parser.parse(new String(jsonString, utf8))}")
        case Right(newCc2) =>
          newCc2.fancyEquals(cc)
      }
    })

  }

}
