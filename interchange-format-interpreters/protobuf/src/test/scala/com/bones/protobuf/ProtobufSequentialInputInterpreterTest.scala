package com.bones.protobuf

import java.io.ByteArrayInputStream
import java.util.UUID

import com.bones.protobuf.messageType.defaultProtoFile
import com.bones.protobuf.values.{defaultEncoder, defaultUtcValidator}
import com.bones.syntax._
import com.google.protobuf.CodedInputStream
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

  test("single items") {

    val denver = Loc("Denver", "CO")
    val bytes =
      defaultEncoder.generateProtobufEncoder(loc)(denver)

    val is = new ByteArrayInputStream(bytes)
    val cin: CodedInputStream = CodedInputStream.newInstance(is)

    val isItDenver =
      defaultUtcValidator.fromCustomBytes(loc)(bytes)

    isItDenver match {
      case Right(l)  => l mustBe denver
      case Left(err) => fail(s"Expected loc, received: ${err}")
    }

  }

  ignore("Person") {

    val result = defaultProtoFile.fromSchemaCustomAlgebra(person)

    val bytes = defaultEncoder.generateProtobufEncoder(person)(monica)

    val isItMonica = defaultUtcValidator.fromCustomBytes(person)(bytes)

    isItMonica match {
      case Right(person) => person mustBe monica
      case Left(err)     => fail(s"Expected monica, received: ${err}")
    }

    val message = defaultProtoFile.messageToProtoFile(result)

    // NOTE! : There are 4 spaces after message Location {}
    val expectedMessage =
      """
        |message Person {
        |  required string id = 1;
        |  required string name = 2;
        |  required int64 age = 3;
        |  required Location location = 4;
        |  required bool knowsAboutGadt = 5;
        |  optional string favoriteColor = 6;
        |
        |
        |  message Location {
        |    required string city = 1;
        |    required string state = 2;
        |  }
        |
        |}
        |     """.stripMargin

    expectedMessage mustEqual message

  }

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

}
