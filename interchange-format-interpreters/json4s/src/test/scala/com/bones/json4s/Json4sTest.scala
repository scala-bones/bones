package com.bones.json4s

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.json4s.values.{isoJson4sEncoderInterpreter, isoJson4sValidatorInterpreter}
import com.bones.scalacheck.values.defaultValuesScalacheck
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

class Json4sTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc = isoJson4sValidatorInterpreter
    .generateValidator[AllSupported](allSupportCaseClass)
  val ccToJson =
    isoJson4sEncoderInterpreter.generateEncoder(allSupportCaseClass)

  implicit val arb = Arbitrary(defaultValuesScalacheck.fromKvpCollection(allSupportCaseClass))
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: AllSupported) => {
      try {
        val json = ccToJson.apply(cc)
        val jsonString = pretty(render(json))
        val newCc = jsonToCc(parse(jsonString))
        newCc match {
          case Left(x) =>
            fail(s"expected success, received $x for JSON string ${compact(render(jsonString))}")
          case Right(newCc2) =>
            newCc2.fancyEquals(cc)
        }
      } catch {
        case ex: Exception => {
          ex.printStackTrace(); fail(ex.getStackTrace.map(_.toString).mkString("\n"))
        }
      }
    })
  }
}
