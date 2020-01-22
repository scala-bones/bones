package com.bones.validation

import com.bones.validation.ValidationDefinition.BigDecimalValidation._
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers
import com.bones.validation.ValidationDefinition.ValidValue
import org.scalatest.MustMatchers
import org.scalatest.funsuite.AnyFunSuite

class BigDecimalValidationTest extends AnyFunSuite with Checkers with MustMatchers{

  test("Max is valid") {
    forAll { (max: BigDecimal, input: BigDecimal) =>
      (max > input) ==> (Max(max).isValid(input))
    }
  }

  test("Max is invalid") {
    forAll { (max: BigDecimal, input: BigDecimal) =>
      (max <= input) ==> (!Max(max).isValid(input))
    }
  }

  test("Max description and default error are acceptable") {
    val max = Max(BigDecimal("7.83"))
    assert(
      max.defaultError(BigDecimal("9.445")) === "9.445 is greater than 7.83")
    assert(max.description === "maximum of 7.83")
  }

  test("Min is valid") {
    forAll { (min: BigDecimal, input: BigDecimal) =>
      (min <= input) ==> (Min(min).isValid(input))
    }
  }

  test("Min is invalid") {
    forAll { (min: BigDecimal, input: BigDecimal) =>
      (min > input) ==> (!Min(min).isValid(input))
    }
  }

  test("Min description and default error are acceptable") {
    val max = Min(BigDecimal("2.77"))
    assert(max
      .defaultError(BigDecimal("1.83")) === "1.83 is less than 2.77")
    assert(max.description === "minimum of 2.77")
  }

  test("Greater than is valid") {
    forAll{ (base: BigDecimal, input: BigDecimal) =>
      (base < input) ==> (Greater(base).isValid(input))
    }
  }

  test("Greater is invalid") {
    forAll{ (base: BigDecimal, input: BigDecimal) =>
      (base >= input) ==> (Greater(base).isValid(input))
    }    
  }

  test("Less than is valid") {
    forAll{ (base: BigDecimal, input: BigDecimal) =>
      (base > input) ==> (Less(base).isValid(input))
    }
  }

  test("Less is invalid") {
    forAll{ (base: BigDecimal, input: BigDecimal) =>
      (base <= input) ==> (Less(base).isValid(input))
    }    
  }

  test("Positive is valid") {
    forAll{ (input: BigDecimal) =>
      (BigDecimal(0) < input) ==> (Positive.isValid(input))
    }
  }

  test("Positive is isValid") {
    forAll{ (input: BigDecimal) =>
      (BigDecimal(0) >= input) ==> (Positive.isValid(input))
    }
  }

  test("Negative is valid") {
    forAll{ (input: BigDecimal) =>
      (BigDecimal(0) > input) ==> (Negative.isValid(input))
    }
  }

  test("Negative is isValid") {
    forAll{ (input: BigDecimal) =>
      (BigDecimal(0) >= input) ==> (Negative.isValid(input))
    }
  }  

  val validValue = Vector(BigDecimal(0), BigDecimal(1), BigDecimal(100))
  val vv = ValidValue(validValue)
  test("Valid Values") {
    vv.isValid(BigDecimal(0.0)) mustBe true
    vv.isValid(BigDecimal(1l)) mustBe true
    vv.isValid(BigDecimal(100)) mustBe true
  }

  test("Valid values not valid input") {
    forAll{ (input: BigDecimal) =>
      (! validValue.contains(input)) ==> (Negative.isValid(input))
    }    
  }


}
