package com.bones.protobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.UUID

import com.bones.protobuf.messageType.{ProtoFileGeneratorInterpreter, defaultProtoFile}
import com.bones.protobuf.values.{defaultEncoder, defaultUtcValidator}
import com.bones.syntax._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers

class ProtobufSequentialInputInterpreterTest extends AnyFunSuite with Checkers with Matchers {

  case class Loc(city: String, state: String)
  case class Person(
    id: UUID,
    name: String,
    age: Long,
    location: Loc,
    knowsAboutGadt: Boolean,
    favoriteColor: Option[String])

  val monica = Person(UUID.randomUUID(), "Monica", 44L, Loc("Denver", "CO"), true, Some("black"))

  val loc = (
    ("city", string) ::
      ("state", string) ::
      kvpNil
  ).convert[Loc]

  val person = (
    ("id", uuid) ::
      ("name", string) ::
      ("age", long) ::
      ("location", loc.asValue) :<:
      ("knowsAboutGadt", boolean) ::
      ("favoriteColor", string.optional) :<:
      kvpNil
  ).convert[Person]

  ignore("single items") {

    val denver = Loc("Denver", "CO")
    val bytes =
      defaultEncoder.generateProtobufEncoder(loc.asValue)(denver)

    val is = new ByteArrayInputStream(bytes)
    val cin: CodedInputStream = CodedInputStream.newInstance(is)

    val isItDenver =
      defaultUtcValidator.fromCustomBytes(loc.asValue)(bytes)

    isItDenver match {
      case Right(l)  => l mustBe denver
      case Left(err) => fail(s"Expected loc, received: ${err}")
    }

  }

  ignore("Person") {

    val os = new ByteArrayOutputStream()
    val cos: CodedOutputStream = CodedOutputStream.newInstance(os)

    val result = defaultProtoFile.fromSchemaCustomAlgebra(person.asValue)

    val str = defaultProtoFile.messageToProtoFile(result)

    val bytes = defaultEncoder.generateProtobufEncoder(person.asValue)(monica)

    val isItMonica = defaultUtcValidator.fromCustomBytes(person.asValue)(bytes)

    isItMonica match {
      case Right(person) => person mustBe monica
      case Left(err)     => fail(s"Expected monica, received: ${err}")
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
