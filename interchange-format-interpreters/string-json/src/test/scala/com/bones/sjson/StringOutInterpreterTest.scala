package com.bones.sjson

import java.time.format.DateTimeFormatter

import com.bones.scalacheck.Scalacheck
import org.scalatestplus.scalacheck.Checkers

import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass, exampleCreditCard}
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite

class StringOutInterpreterTest extends FunSuite with Checkers {


  val interpreter = new StringOutInterpreter {
    override val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }


  val ccF = interpreter.valueDefinition(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass))

  test("to json") {

    check((cc: AllSupported) => {
      val json = ccF(cc)
//      println("============")
//      println(json)
      true
    })
  }

}
