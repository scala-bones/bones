package com.bones.sprayjson

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.scalacheck.values.defaultValuesScalacheck
import com.bones.schemas.Schemas.AllSupported
import com.bones.schemas.WithLongId
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import values._
import spray.json.DefaultJsonProtocol._
import spray.json.{JsValue, _}

class SchemaWithIdTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val jsonToCc =
    isoSprayValidatorInterpreter.generateByteArrayValidator[(Long, AllSupported)](
      WithLongId.allSupportedWithId,
      StandardCharsets.UTF_8
    )
  val ccToJson =
    isoSprayEncoderInterpreter.generateEncoder(WithLongId.allSupportedWithId)

  implicit val arb = Arbitrary(
    defaultValuesScalacheck
      .valueDefinition(WithLongId.allSupportedWithId.asValue)
  )
  val utf8 = Charset.forName("UTF8")

  test("scalacheck allSupport types - marshall then unmarshall") {
    check((cc: (Long, AllSupported)) => {
      val json = ccToJson.encode(cc)
      val jsonString = json.prettyPrint.getBytes(utf8)
      val newCc = jsonToCc(jsonString)
      newCc match {
        case Left(x) =>
          fail(
            s"expected success, received $x for JSON string ${new String(jsonString, utf8).toJson}"
          )
        case Right(newCc2) =>
          newCc2._2.fancyEquals(cc._2)
          newCc2._1 == cc._1
      }
    })
  }
}
