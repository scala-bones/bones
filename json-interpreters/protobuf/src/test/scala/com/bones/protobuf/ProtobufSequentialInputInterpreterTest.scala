package com.bones.protobuf

import com.bones.data.Value.KvpNil
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import com.bones.syntax._
import com.google.protobuf.CodedInputStream

class ProtobufSequentialInputInterpreterTest extends FunSuite with Checkers {

  case class Loc(city: String, state: String)
  case class Person(name: String, age: String, location: Loc)

  val person = (
    kvp("name", string) ::
    kvp("age", string) ::
    kvpValue("location", (
      kvp("city", string) ::
      kvp("state", string) ::
      KvpNil
    ).convert[Loc]) ::
    KvpNil
  ).convert[Person]

//  val buff: Array[Byte] = ???
//  val cin: CodedInputStream = CodedInputStream.newInstance(buff)
//  ProtobufSequentialInputInterpreter.dataClass(person)(0,Vector.empty)._2(cin)

}
