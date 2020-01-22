package com.bones.validation

import java.util.UUID

import com.bones.validation.ValidationDefinition.StringValidation._
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers

class ValidationDefinitionTest extends FunSuite with Checkers {

  test("valid values") {
    val validValues = Vector("The", "Quick", "Brown", "Fox")
    val validTest = valid(validValues: _*)

    validValues.foreach(v => {
      assert(validTest.isValid(v) === true)
    })

    List("A", "1", "brown").foreach(v => {
      assert(validTest.isValid(v) === false)
    })

    val invalidTest = invalid(validValues: _*)

    validValues.foreach(v => {
      assert(invalidTest.isValid(v) === false)
    })
    List("there's", "more", "to", "life", "than", "this").foreach(v => {
      assert(invalidTest.isValid(v) === true)
    })
  }

  test("string validation") {

    List("abc", "a1v2s3", "1a2v3s").foreach(i => {
      assert(alphanumeric.isValid(i) === true)
    })

    List("a?a", "8*8", "(0)").foreach(i => {
      assert(alphanumeric.isValid(i) === false)
    })

  }

  test("matches regex") {
    val s = matchesRegex("abcdefg".r)

    assert(s.isValid("abcdefg") === true)
    assert(s.isValid("12345") === false)
  }

  test("lengthO") {
    val s = length(5)
    assert(s.isValid("12345") === true)
    assert(s.isValid("1234") === false)
    assert(s.isValid("123456") === false)
  }

  test("custom") {
    val v = custom(_ => true, (v: String) => s"${v} default", "description")
    assert(v.isValid("x") === true)
    assert(v.defaultError("test") === "test default")
    assert(v.description === "description")

    val v2 = custom(_ => false, identity, "")
    assert(v2.isValid("x") === false)

  }

  test("guid") {
    val v = guid
    assert(v.isValid(UUID.randomUUID().toString) === true)
    assert(v.isValid("123") === false)
  }

  test("email") {
    val e = email
    assert(e.isValid("billy.blanks@example.com") === true)
    assert(e.isValid("the quick brown fox") === false)
  }

  test("token") {
    val v = token
    assert(v.isValid("the_quick_brown_fox") === true)
    assert(v.isValid("the quick brown fox") === false)
  }

  test("hex") {
    val v = hex
    assert(hex.isValid("A1B2C3E4F567890") === true)
    assert(hex.isValid("ABCG") === false)
  }

  test("base64") {
    pending
    val v = base64
    assert(v.isValid("A1B2C3E4F567890$") === true)
    assert(v.isValid("¢¢£") === false)
  }

  test("hostname") {
    val v = hostname
    assert(v.isValid("www.oletraveler.com") === true)
    assert(v.isValid("the quick brown fox") === false)
  }

  test("ipv4") {
    val v = iPv4
    assert(v.isValid("1.1.1.1") === true)
    assert(v.isValid("the quick") === false)
  }

  test("lowercase") {
    val v = lowercase
    assert(v.isValid("a quick brown fox!") === true)
    assert(v.isValid("a Quick Brown Fox!") === false)
  }

  test("uri") {
    val v = uri
    assert(v.isValid("https://www.oletraveler.com") === true)
    assert(v.isValid("the quick brown fox") === false)
  }

  test("creditCard") {
    val v = creditCard
    assert(v.isValid("5454545454545454") === true)
    assert(v.isValid("the quick") === false)
  }

  test("words") {
    val w = words
    assert(w.isValid("the quick brown fox") === true)
    assert(w.isValid("jump43245") === false)
  }

  test("sentences") {
    assert(sentence.isValid("The quick brown fox jumps over the lazy dog.") === true)
    assert(sentence.isValid("ljdlfjas0808934@#$F") === false)
  }
}
