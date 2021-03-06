package com.bones.circe

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.circe.values._
import com.bones.scalacheck.values.defaultValuesScalacheck
import com.bones.schemas.Schemas.AllSupported
import com.bones.schemas.WithLongId
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers

class SchemaWithIdTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc =
    CirceValidatorFromByteArray(StandardCharsets.UTF_8)
      .andThen(isoCirceValidatorInterpreter.generateValidator(WithLongId.allSupportedWithId))

  val ccToJson =
    isoCirceEncoderInterpreter.generateEncoder(WithLongId.allSupportedWithId)

  implicit val arb = Arbitrary(
    defaultValuesScalacheck
      .valueDefinition(WithLongId.allSupportedWithId.asValue))
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: (Long, AllSupported)) => {
      val json = ccToJson.encode(cc)
      val jsonString = json.spaces2.getBytes(utf8)
      val newCc = jsonToCc.validate(jsonString)
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
