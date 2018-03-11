package com.ot.bones.validation

import com.ot.bones.BigDecimalValidation._
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

class BigDecimalValidationTest extends FunSuite with Checkers {

  test("Max is valid") {
    forAll { (max: BigDecimal, input: BigDecimal) =>
      (max > input) ==> (Max(max).isValid(input))
    }
  }

  test("Max is invalid") {
    forAll { (max: BigDecimal, input: BigDecimal) =>
      (max <= input) ==> (! Max(max).isValid(input))
    }
  }

  test("Max description and default error are acceptable") {
    val max = Max(BigDecimal("7.83"))
    assert( max.defaultError(BigDecimal("9.445")) === "9.445 is greater than the maximum 7.83" )
    assert( max.description === "maximum value of 7.83" )
  }

  test("Min is valid") {
    forAll{ (min: BigDecimal, input: BigDecimal) =>
      (min <= input) ==> (Min(min).isValid(input))
    }
  }

  test("Min is invalid") {
    forAll{ (min: BigDecimal, input: BigDecimal) =>
      (min > input) ==> (! Min(min).isValid(input))
    }
  }

  test("Min description and default error are acceptable") {
    val max = Min(BigDecimal("2.77"))
    assert( max.defaultError(BigDecimal("1.83")) === "1.83 is less than the minimum 2.77" )
    assert( max.description === "minimum value of 2.77" )
  }


}
