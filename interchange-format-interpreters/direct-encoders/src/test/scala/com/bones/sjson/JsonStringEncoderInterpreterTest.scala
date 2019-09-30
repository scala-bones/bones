package com.bones.sjson

import java.time.format.DateTimeFormatter

import com.bones.scalacheck.Scalacheck
import org.scalatestplus.scalacheck.Checkers

import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite

class JsonStringEncoderInterpreterTest extends FunSuite with Checkers {


  val interpreter = new JsonStringEncoderInterpreter {
    override val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }


  val ccF = interpreter.valueDefinition(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass))

  test("to json") {

    check((cc: AllSupported) => {
      val json = ccF(cc)
      json match {
        case Some(v) => succeed
        case None => fail("expected success")
      }
      true
    })
  }

}
