package com.bones.protobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer

import com.bones.data.Value.KvpNil
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.Checkers
import com.bones.syntax._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

class ProtobufSequentialInputInterpreterTest extends FunSuite with Checkers with MustMatchers{

  case class Loc(city: String, state: String)
  case class Person(name: String, age: Long, location: Loc)

  val monica = Person("Monica", 44l, Loc("Denver", "CO"))

  val person = (
    kvp("name", string) ::
    kvp("age", long) ::
    kvpValue("location", (
      kvp("city", string) ::
      kvp("state", string) ::
      KvpNil
    ).convert[Loc]) ::
    KvpNil
  ).convert[Person]

  val os = new ByteArrayOutputStream()
  val cos: CodedOutputStream = CodedOutputStream.newInstance(os)

//  val outF = ProtobufSequentialOutputInterpreter.dataClass(person)(1)
//  outF.apply(monica, cos)
//  cos.flush()
//  os.flush()
//  os.close()
//
//  val is = new ByteArrayInputStream(os.toByteArray)
//  val cin: CodedInputStream = CodedInputStream.newInstance(is)
//  val isItMonica = ProtobufSequentialInputInterpreter.dataClass(person)(0,Vector.empty)._2(cin)
//
//  isItMonica match {
//    case Right(person) => person mustBe monica
//    case Left(err) => fail(s"Expected monica, received: ${err}")
//  }

}
