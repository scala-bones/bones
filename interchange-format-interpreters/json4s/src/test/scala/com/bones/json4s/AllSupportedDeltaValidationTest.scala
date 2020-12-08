package com.bones.json4s

import com.bones.schemas.Schemas
import org.json4s._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class AllSupportedDeltaValidationTest extends AnyFunSuite with Checkers {

  test("all supported delta") {

    val alg = Schemas.allSupportedSchema
    val deltaValidator =
      com.bones.json4s.values.isoJson4sDeltaValidatorInterpreter.fromKvpHListCollection(alg)

    deltaValidator.hListRR

    deltaValidator.validate(JObject(List.empty), List.empty) match {
      case Left(err) => fail(s"unexpected errors: ${err}")
      case Right(h) => {
        succeed
      }
    }

    val someValues = JObject(("long", JLong(55)), ("string", JString("Hello World")))
    deltaValidator.validate(someValues, List.empty) match {
      case Left(err) => fail(s"unexpected errors: ${err}")
      case Right(h) => {
        h
      }
    }

    val invalidValues = JObject(("long", JLong(-55)), ("int", JInt(1000)))
    deltaValidator.validate(invalidValues, List.empty) match {
      case Left(err) => println(err)
      case Right(h)  => fail("Expected errors")
    }

  }

}
