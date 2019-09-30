package com.bones.bson

import java.nio.charset.Charset
import java.util

import com.bones.scalacheck.Scalacheck
import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers
import com.bones.schemas.Schemas._
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.buffer.{ArrayBSONBuffer, ArrayReadableBuffer, ReadableBuffer, WritableBuffer}


class BsonTest extends FunSuite with Checkers with MustMatchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val bsonToCc = BsonValidatorInterpreter.fromSchema(allSupportCaseClass)

  val ccToBson = BsonEncoderInterpreter.fromSchema(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass))

  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {
      val buffer = new ArrayBSONBuffer()
      ccToBson(cc) match {
        case doc: BSONDocument => BSONDocument.write(doc, buffer)
        case x => fail(s"expected BSONDocument, received $x")
      }
      val bytes = buffer.array

      val readBuffer = ArrayReadableBuffer.apply(bytes)
      val doc = BSONDocument.read(readBuffer)


      val newCc = bsonToCc.apply(doc, List.empty)

      newCc match {
        case Left(x) =>
          fail(s"expected success, received $x for BSON bytes ${bytes}")
        case Right(newCc2) =>
          val nullBa = Array[Byte]()

          //Arrays seem to only be equal when they reference the same object, so let's remove them form the whole object copy
          val newCc2NoBa = newCc2.copy(ba = nullBa).copy(child = newCc2.child.copy(ba = None))
          val ccNoBA = cc.copy(ba = nullBa).copy(child = cc.child.copy(ba = None))
          newCc2NoBa == ccNoBA && java.util.Arrays.equals(newCc2.ba, cc.ba)
      }
    })

  }

}
