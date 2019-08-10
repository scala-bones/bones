package com.bones.interpreter

import com.bones.data.Value.KvpNil
import com.bones.syntax._
import net.liftweb.json.JsonAST.{JField, JInt, JObject, JString}
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import shapeless.{::, HNil}

class ValidatedFromJObjectInterpreterTest extends FunSuite with Checkers {

  val interpreter = ValidatedFromJObjectInterpreter.isoDates

  test("json to string") {
    val testField = kvp("test", string()) :: KvpNil
    val validated = interpreter.kvpHList(testField)

    val input = JObject(JField("test", JString("Hello World")))
    val output = validated.apply(input, List.empty)

    output match {
      case Right(str) => assert( str.head === "Hello World" )
      case Left(x) => fail(s"expected valid, received $x")
    }
  }

  test ("either") {
    val eitherDesc = kvp("test", either(string, long)) :: KvpNil
    val prog = interpreter.kvpHList(eitherDesc)

    val validInput = JObject(JField("test", JString("Hello String")))

    val output = prog(validInput, List.empty)
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
    val intOutput = prog.apply(validIntInput, List.empty)
    intOutput match {
      case Right(Right(i) :: HNil) => assert(i === 42)
      case x => fail(s"expected right, right, received: $x")
    }

  }

}
