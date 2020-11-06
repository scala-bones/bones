package com.bones.sprayjson

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.scalacheck.values.defaultValuesScalacheck
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import values._
import spray.json._
import DefaultJsonProtocol._

class SprayTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc = isoSprayValidatorInterpreter
    .generateByteArrayValidator[AllSupported](allSupportCaseClass, StandardCharsets.UTF_8)
  val ccToJson =
    isoSprayEncoderInterpreter.generateEncoder(allSupportCaseClass)

  implicit val arb = Arbitrary(defaultValuesScalacheck.fromKvpCollection(allSupportCaseClass))
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: AllSupported) => {
      try {
        val json = ccToJson.apply(cc)
        val jsonString = json.prettyPrint.getBytes(utf8)
        val newCc = jsonToCc(jsonString)
        newCc match {
          case Left(x) =>
            fail(
              s"expected success, received $x for JSON string ${new String(jsonString, utf8).toJson}")
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
