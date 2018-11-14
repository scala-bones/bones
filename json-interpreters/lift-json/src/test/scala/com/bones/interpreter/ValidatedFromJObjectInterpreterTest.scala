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
    val str = kvp("test", string()) :: KvpNil
    val validated = interpreter.apply(str)

    val input = JObject(JField("test", JString("Hello World")))
    val output = validated.apply(Some(input))

    output match {
      case Right(str) => assert( str.head === "Hello World" )
      case Left(x) => fail(s"expected valid, received $x")
    }
  }

  test ("either") {
    val eitherDesc = kvp("test", either(string, int)) :: KvpNil
    val prog = interpreter(eitherDesc)

    val validInput = JObject(JField("test", JString("Hello String")))

    val output = prog.apply(Some(validInput))
    output match {
      case Right(r) => {
        val head = r.head
        val left = head.left
        val op = left.toOption
        assert(op === Some("Hello String"))
      }
      case x => fail(s"expected, right, received $x")
    }

    val validIntInput = JObject(JField("test", JInt(42)))
    val intOutput = prog.apply(Some(validIntInput))
    intOutput match {
      case Right(Right(i) :: HNil) => assert(i === 42)
      case x => fail(s"expected right, right, received: $x")
    }

  }

}
