package com.bones.sjson

import java.time.format.DateTimeFormatter

import com.bones.scalacheck.{NoAlgebraGen, Scalacheck}
import org.scalatestplus.scalacheck.Checkers
import com.bones.schemas.Schemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.FunSuite

class JsonStringEncoderInterpreterTest extends FunSuite with Checkers {


  val interpreter = new JsonStringEncoderInterpreter {
    override val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }


  val ccF = interpreter.valueDefinition(allSupportCaseClass, JsonStringEncoderInterpreter.NoAlgebra)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass, NoAlgebraGen))

  test("to json") {

    check((cc: AllSupported) => {
      val json = ccF(cc)
      json match {
        case Some(str) =>
          io.circe.parser.parse(str) match {
            case Left(err) => {
              fail(err.toString + str)
            }
            case Right(_) => succeed
          }
        case None => fail("expected success")
      }
      true
    })
  }

}
