package com.bones.validation.values

import java.util.UUID

import com.bones.data.values.CustomStringValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class CustomStringValidatorTest extends AnyFunSuite with Checkers {


  test("guid") {
    val v = CustomStringValue.GuidDataValidationOp
    assert(v.isValid(UUID.randomUUID().toString) === true)
    assert(v.isValid("123") === false)
  }

  test("email") {
    val e = CustomStringValue.EmailDataValidationOp
    assert(e.isValid("billy.blanks@example.com") === true)
    assert(e.isValid("the quick brown fox") === false)
  }

  test("hex") {
    val hex = CustomStringValue.HexStringValidationOp
    assert(hex.isValid("A1B2C3E4F567890") === true)
    assert(hex.isValid("ABCG") === false)
  }

  test("base64") {
    pending
    val v = CustomStringValue.Base64ValidationOp
    assert(v.isValid("A1B2C3E4F567890$") === true)
    assert(v.isValid("¢¢£") === false)
  }

  test("hostname") {
    val v = CustomStringValue.HostnameValidationOp
    assert(v.isValid("www.oletraveler.com") === true)
    assert(v.isValid("the quick brown fox") === false)
  }

  test("ipv4") {
    val v = CustomStringValue.Ipv4ValidationOp
    assert(v.isValid("1.1.1.1") === true)
    assert(v.isValid("the quick") === false)
  }

  test("uri") {
    val v = CustomStringValue.UriValidationOp
    assert(v.isValid("https://www.oletraveler.com") === true)
    assert(v.isValid("the quick brown fox") === false)
  }

  test("creditCard") {
    val v = CustomStringValue.CreditCardValidationOp
    assert(v.isValid("5454545454545454") === true)
    assert(v.isValid("the quick") === false)
  }
}
