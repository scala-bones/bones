package com.bones.protobuf

import java.util.Base64

import com.bones.scalacheck.Scalacheck
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.{AllSupported, CC, allSupportCaseClass}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers


class ProtobufScalacheckTest extends FunSuite with Checkers {

  val encode = ProtobufSequentialOutputInterpreter.encodeToBytes(Schemas.allSupportCaseClass)
  val decode = ProtobufSequentialInputInterpreter.fromBytes(Schemas.allSupportCaseClass)

//  implicit override val generatorDrivenConfig =
//    PropertyCheckConfiguration(minSuccessful = 100, workers = 5)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass))

  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {

      val bytes = encode(cc)

      val newCc = try {
        decode(bytes)
      } catch {
        case ex: Exception => ex.printStackTrace()
        throw ex
      }

      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for protobuff bytes ${bytes}")
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
