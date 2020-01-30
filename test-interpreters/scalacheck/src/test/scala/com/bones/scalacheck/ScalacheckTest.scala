package com.bones.scalacheck

import com.bones.data.KvpNil
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.{AllSupported, CC}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers
import com.bones.syntax._

class ScalacheckTest extends FunSuite with Checkers {

  test("int") {
    val intSchema = int(iv.between(0, 100))
    implicit val arbInt = Arbitrary(Scalacheck.valueDefinition(intSchema, NoAlgebraGen))
    check((i: Int) => i >= 0 && i <= 100)
  }

  test("check scalacheck") {
    implicit val gen: Gen[CC] = Scalacheck.valueDefinition(Schemas.creditCardSchema, NoAlgebraGen)
    implicit val arb = Arbitrary(gen)
    check((cc: CC) => { true })
  }

  /** Check we gen all supported types */
  test("all supported") {
    implicit val allSupportedGen =
      Scalacheck.valueDefinition(Schemas.allSupportCaseClass, NoAlgebraGen)
    implicit val arb = Arbitrary(allSupportedGen)

    check((a: AllSupported) => { true })
  }

}
