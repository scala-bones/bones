package com.bones.json4s

import java.nio.charset.Charset

import com.bones.json4s.values.{isoJson4sEncoderInterpreter, isoJson4sValidatorInterpreter}
import com.bones.scalacheck.values.defaultValuesScalacheck
import com.bones.schemas.Schemas.AllSupported
import com.bones.schemas.WithLongId
import org.json4s.JsonDSL._
import org.json4s._
import org.json4s.native.JsonMethods._
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers

class SchemaWithIdTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc =
    isoJson4sValidatorInterpreter.generateValidator[(Long, AllSupported)](
      WithLongId.allSupportedWithId
    )
  val ccToJson =
    isoJson4sEncoderInterpreter.generateEncoder(WithLongId.allSupportedWithId)

  implicit val arb = Arbitrary(
    defaultValuesScalacheck
      .valueDefinition(WithLongId.allSupportedWithId.asValue)
  )
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: (Long, AllSupported)) => {
      val json = ccToJson.encode(cc)
      val jsonString = pretty(render(json))
      val newCc = jsonToCc.validate(parse(jsonString))
      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for JSON string ${compact(render(jsonString))}")
        case Right(newCc2) =>
          newCc2._2.fancyEquals(cc._2)
          newCc2._1 == cc._1
      }
    })
  }
}
