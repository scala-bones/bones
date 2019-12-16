package com.bones.argonaut

import java.nio.charset.Charset

import com.bones.scalacheck.Scalacheck
import com.bones.schemas.Schemas._
import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers


class ArgonautTest extends FunSuite with Checkers with MustMatchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val validateFromCirce = ArgonautValidatorInterpreter.isoInterpreter

  val jsonToCc = validateFromCirce.byteArrayFuncFromSchema(allSupportCaseClass, Charset.forName("UTF8"))
  val ccToJson = ArgonautEncoderInterpreter.isoInterpreter.fromSchema(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.createGen(allSupportCaseClass))
  val utf8 = Charset.forName("UTF8")


  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {
      val json = ccToJson.apply(cc)
      val jsonString = json.spaces2.getBytes(utf8)
      val newCc = jsonToCc(jsonString)
      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for JSON string ${argonaut.Parse.parse(new String(jsonString, utf8))}")
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
