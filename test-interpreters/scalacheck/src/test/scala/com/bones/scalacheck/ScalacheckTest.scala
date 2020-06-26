package com.bones.scalacheck

import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.{AllSupported, CC}
import com.bones.syntax._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import com.bones.scalacheck.custom._

class ScalacheckTest extends AnyFunSuite with Checkers {

  test("int") {
    val intSchema = int(iv.between(0, 100))
    implicit val arbInt = Arbitrary(Scalacheck.determineValueDefinition(Right(intSchema), allInterpreters))
    check((i: Int) => i >= 0 && i <= 100)
  }

  test("check scalacheck") {
    implicit val gen: Gen[CC] = Scalacheck.valueDefinition(Schemas.creditCardSchema, allInterpreters)
    implicit val arb = Arbitrary(gen)
    check((cc: CC) => { true })
  }

  /** Check we gen all supported types */
  test("all supported") {
    implicit val allSupportedGen =
      Scalacheck.valueDefinition(Schemas.allSupportCaseClass, allInterpreters)
    implicit val arb = Arbitrary(allSupportedGen)

    check((a: AllSupported) => { true })
  }

}
