package com.bones.sjson

import java.time.format.DateTimeFormatter

import com.bones.scalacheck.{Scalacheck}
import org.scalatestplus.scalacheck.Checkers
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite

class JsonStringEncoderInterpreterTest extends AnyFunSuite with Checkers {

  val interpreter = JsonStringEncoderInterpreter.isoEncoder

  val ccF = interpreter.valueDefinition(allSupportCaseClass, com.bones.sjson.values.allEncoders)

  implicit val arb: Arbitrary[AllSupported] = Arbitrary(
    Scalacheck.valueDefinition(allSupportCaseClass, com.bones.scalacheck.values.allInterpreters))

  test("to json") {

    check((cc: AllSupported) => {
      val json = ccF(cc)
      val result =
        if (json.isEmpty)
          fail("expected success")
        else {
          val str = json.mkString
          io.circe.parser.parse(str) match {
            case Left(err) => fail(err.toString + str)
            case Right(_)  => succeed
          }
        }
      result === succeed
    })
  }

}
