package com.bones.circe

import java.nio.charset.Charset

import com.bones.scalacheck.Scalacheck
import org.scalatest.{FunSuite, MustMatchers}
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers



class CirceTest extends FunSuite with Checkers with MustMatchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val validateFromCirce = CirceValidatorInterpreter.isoInterpreter

  val jsonToCc = validateFromCirce.byteArrayFuncFromSchema(allSupportCaseClass, Charset.forName("UTF8"))
  val ccToJson = CirceEncoderInterpreter.isoInterpreter.fromSchema(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass))
  val utf8 = Charset.forName("UTF8")


  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {
      val json = ccToJson.apply(cc)
      val jsonString = json.spaces2.getBytes(utf8)
      val newCc = jsonToCc(jsonString)
      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for JSON string ${io.circe.parser.parse(new String(jsonString, utf8))}")
        case Right(newCc2) =>
          val nullBa = Array[Byte]()

          //Arrays seem to only be equal when they reference the same object, so let's remove them form the whole object copy
          val newCc2NoBa = newCc2.copy(ba = nullBa).copy(child = newCc2.child.copy(ba = None))
          val ccNoBA = cc.copy(ba = nullBa).copy(child = cc.child.copy(ba = None))
          newCc2NoBa == ccNoBA && java.util.Arrays.equals(newCc2.ba, cc.ba)
      }
    })

  }

}
