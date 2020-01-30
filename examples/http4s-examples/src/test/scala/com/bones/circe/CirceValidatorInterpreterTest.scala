package com.bones.circe

import com.bones.data.KvpNil
import com.bones.syntax._
import io.circe.{Json => CJson}
import org.scalatest.FunSuite
import io.circe.parser.parse
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter

class CirceValidatorInterpreterTest extends FunSuite {

  val json: String = """
  {
    "id": "c730433b-082c-4984-9d66-855c243266f0",
    "name": "Foo",
    "counts": [1, 2, 3],
    "values": {
      "bar": true,
      "baz": 100.001,
      "qux": ["a", "b"]
    }
  }
  """
  val circeDoc: CJson = parse(json).getOrElse(CJson.Null)

//  val liftJson = net.liftweb.json.parse(""" { "numbers" : [1, 2, 3, 4] } """)




  test("kvp String") {
    val str = ("name", string(sv.length(3))) :<: kvpNil

    CirceValidatorInterpreter
      .isoInterpreter
      .kvpHList(str, KvpInterchangeFormatValidatorInterpreter.NoAlgebraValidator()).apply(circeDoc, List.empty) match {
        case Left(err) => fail(s"expected success, received: ${err}")
        case Right(r) => assert(r.head === "Foo")
      }

  }

  test( "kvp String fail validation") {
    val str = ("name", string(sv.length(2))) :<: kvpNil

    CirceValidatorInterpreter.isoInterpreter.kvpHList(str, KvpInterchangeFormatValidatorInterpreter.NoAlgebraValidator()).apply(circeDoc, List.empty) match {
      case Left(err) => succeed
      case Right(r) => fail(s"expected validation failure, received: ${r}")
    }

  }

  // test("kvp BigDecimal") {
  //   val bd =
  //     ("values", ("baz", bigDecimal(bdv.Min(BigDecimal(0)))) :: kvpNil ) ::: kvpNil

  //   CirceValidatorInterpreter.isoInterpreter.kvpHList(bd, KvpInterchangeFormatValidatorInterpreter.NoAlgebraValidator()).apply(circeDoc, List.empty) match {
  //     case Left(err) => fail(s"expected success, received: ${err}")
  //     case Right(r) => assert(r.head.head == BigDecimal(100.001))
  //   }
  // }

}
