package com.bones.interpreter

import cats.data.Validated.{Invalid, Valid}
import com.bones.data.Value.{IntData, KvpNil, StringData}
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.Checkers
import com.bones.syntax._
import net.liftweb.json.JsonAST.{JField, JInt, JObject, JString}
import shapeless.{::, HNil}

class ValidatedFromJObjectInterpreterTest extends FunSuite with Checkers {

  val interpreter = new ValidatedFromJObjectInterpreter()

  test("json to string") {
    val str = key("test").string() :: KvpNil
    val validated = interpreter.apply(str)

    val input = JObject(JField("test", JString("Hello World")))
    val output = validated.apply(input)

    output match {
      case Valid(str) => assert( str.head === "Hello World" )
      case Invalid(x) => fail(s"expected valid, received $x")
    }
  }

  test ("either") {
    val eitherDesc = key("test").either(StringData(), IntData()) :: KvpNil
    val prog = interpreter(eitherDesc)

    val validInput = JObject(JField("test", JString("Hello String")))

    val output = prog.apply(validInput)
    output.toEither match {
      case Right(r) => assert(r.head.left.toOption === Some("Hello String"))
      case x => fail("expected valid, right")
    }

    val validIntInput = JObject(JField("test", JInt(42)))
    val intOutput = prog.apply(validIntInput)
    intOutput match {
      case Valid(Right(i) :: HNil) => assert(i === 42)
      case x => fail(s"expected valid, left, received: $x")
    }

  }

}
