package com.bones.circe

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.scalacheck.{NoAlgebraGen, Scalacheck}
import com.bones.schemas.Schemas.AllSupported
import com.bones.schemas.WithId
import com.bones.syntax.NoAlgebra
import org.scalacheck.Arbitrary
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers

class SchemaWithIdTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc = IsoCirceEncoderAndValidatorInterpreter.byteArrayFuncFromSchema[NoAlgebra,(Long,AllSupported)](
    WithId.allSupportedWithId,
    StandardCharsets.UTF_8,
    noAlgebraValidator)
  val ccToJson = IsoCirceEncoderAndValidatorInterpreter.encoderFromSchema(WithId.allSupportedWithId)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(WithId.allSupportedWithId, NoAlgebraGen))
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: (Long,AllSupported)) => {
      val json = ccToJson.apply(cc)
      val jsonString = json.spaces2.getBytes(utf8)
      val newCc = jsonToCc(jsonString)
      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for JSON string ${io.circe.parser
            .parse(new String(jsonString, utf8))}")
        case Right(newCc2) =>
          newCc2._2.fancyEquals(cc._2)
          newCc2._1 == cc._1
      }
    })
  }
}
