package com.bones.protobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.ByteBuffer
import java.util.{Base64, UUID}

import com.bones.data.Value.KvpNil
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatest.prop.Checkers
import com.bones.syntax._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

class ProtobufSequentialInputInterpreterTest extends FunSuite with Checkers with MustMatchers{

  case class Loc(city: String, state: String)
  case class Person(id: UUID, name: String, age: Long, location: Loc, knowsAboutGadt: Boolean, favoriteColor: Option[String])

  val monica = Person(UUID.randomUUID(), "Monica", 44l, Loc("Denver", "CO"), true, Some("black"))

  val loc = (
    kvp("city", string) ::
      kvp("state", string) ::
      KvpNil
  ).convert[Loc]

  val person = (
    kvp("id", uuid) ::
    kvp("name", string) ::
    kvp("age", long) ::
    kvp("location", loc) ::
    kvp("knowsAboutGadt", boolean) ::
    kvp("favoriteColor", string.optional) ::
    KvpNil
  ).convert[Person]

  ignore("single items") {

    val denver = Loc("Denver", "CO")
    val bytes = ProtobufSequentialOutputInterpreter.encodeToBytes(loc)(denver)

    val is = new ByteArrayInputStream(bytes)
    val cin: CodedInputStream = CodedInputStream.newInstance(is)

    val isItDenver = ProtobufSequentialInputInterpreter.fromBytes(loc)(bytes)

    isItDenver match {
      case Right(l) => l mustBe denver
      case Left(err) => fail(s"Expected loc, received: ${err}")
    }



  }

  ignore("Person") {

    val os = new ByteArrayOutputStream()
    val cos: CodedOutputStream = CodedOutputStream.newInstance(os)

    val result = ProtoFileInterpreter.fromSchema(person)

    val str = ProtoFileInterpreter.messageToProtoFile(result)

    val bytes = ProtobufSequentialOutputInterpreter.encodeToBytes(person)(monica)

    println("result:" + Base64.getEncoder.encodeToString(bytes))

    val isItMonica = ProtobufSequentialInputInterpreter.fromBytes(person)(bytes)

    isItMonica match {
      case Right(person) => person mustBe monica
      case Left(err) => fail(s"Expected monica, received: ${err}")
    }
  }

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

}
