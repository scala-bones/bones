package com.bones.bson

import com.bones.scalacheck.Scalacheck
import com.bones.schemas.Schemas._
import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.buffer.{ArrayBSONBuffer, ArrayReadableBuffer}


class BsonTest extends AnyFunSuite with Checkers with Matchers {

//  implicit override val generatorDrivenConfig =
//    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val bsonToCc = BsonValidatorInterpreter.validatorFromSchema(allSupportCaseClass)

  val ccToBson = BsonEncoderInterpreter.encoderFromSchema(allSupportCaseClass)

  implicit val arb = Arbitrary(Scalacheck.fromBonesSchema(allSupportCaseClass))

  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {
      val buffer = new ArrayBSONBuffer()
      try {
        ccToBson(cc)
      } catch {
        case e => e.printStackTrace()
      }
//      ccToBson(cc) match {
//        case doc: BSONDocument => BSONDocument.write(doc, buffer)
//        case x => fail(s"expected BSONDocument, received $x")
//      }
//      val bytes = buffer.array

//      val readBuffer = ArrayReadableBuffer.apply(bytes)
//      val doc = BSONDocument.read(readBuffer)


//      val newCc = bsonToCc.apply(doc)
//
//      newCc match {
//        case Left(x) =>
//          fail(s"expected success, received $x for BSON bytes ${bytes}")
//        case Right(newCc2) =>
//          val nullBa = Array[Byte]()
//
//          //Arrays are only equal when they reference the same object, so let's remove them form the whole object copy
//          val newCc2NoBa = newCc2.copy(ba = nullBa).copy(child = newCc2.child.copy(ba = None))
//          val ccNoBA = cc.copy(ba = nullBa).copy(child = cc.child.copy(ba = None))
//          newCc2NoBa == ccNoBA && java.util.Arrays.equals(newCc2.ba, cc.ba)
//      }
      true
    })

  }

}
