package com.bones.validation

import com.bones.validation.ValidationDefinition.StringValidation.{custom => customStrValidation, _}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers

class ValidationDefinitionTest extends AnyFunSuite with Checkers {

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
    val v = customStrValidation(_ => true, (v: String) => s"${v} default", "description")
    assert(v.isValid("x") === true)
    assert(v.defaultError("test") === "test default")
    assert(v.description === "description")

    val v2 = customStrValidation(_ => false, identity, "")
    assert(v2.isValid("x") === false)

  }

  test("token") {
    val v = token
    assert(v.isValid("the_quick_brown_fox") === true)
    assert(v.isValid("the quick brown fox") === false)
  }

  test("lowercase") {
    val v = lowercase
    assert(v.isValid("a quick brown fox!") === true)
    assert(v.isValid("a Quick Brown Fox!") === false)
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
