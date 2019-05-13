package com.bones.scalacheck

import com.bones.data.Value.KvpNil
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.CC
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers
import com.bones.syntax._

class ScalacheckTest extends FunSuite with Checkers {

  test("int") {
    val intSchema = int(iv.between(0,100))
    implicit val arbInt = Arbitrary(Scalacheck.valueDefinition(intSchema))
    check( (i: Int) => i >= 0 && i <= 100)
  }

  test("check scalacheck") {
    implicit val gen: Gen[CC] = Scalacheck.valueDefinition(Schemas.creditCardSchema)
    implicit val arb = Arbitrary(gen)
    check( (cc: CC) => { true } )
  }


}
