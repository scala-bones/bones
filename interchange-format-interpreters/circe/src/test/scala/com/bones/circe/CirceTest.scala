package com.bones.circe

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.scalacheck.{NoAlgebraGen, Scalacheck}
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import com.bones.syntax.NoAlgebra
import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers

class CirceTest extends FunSuite with Checkers with MustMatchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc = IsoCirceEncoderAndValidatorInterpreter.byteArrayFuncFromSchema[NoAlgebra,AllSupported](
    allSupportCaseClass,
    StandardCharsets.UTF_8,
    noAlgebraValidator)
  val ccToJson = IsoCirceEncoderAndValidatorInterpreter.encoderFromSchema(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass, NoAlgebraGen))
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: AllSupported) => {
      val json = ccToJson.apply(cc)
      val jsonString = json.spaces2.getBytes(utf8)
      val newCc = jsonToCc(jsonString)
      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for JSON string ${io.circe.parser
            .parse(new String(jsonString, utf8))}")
        case Right(newCc2) =>
          newCc2.fancyEquals(cc)
      }
    })

  }

}
