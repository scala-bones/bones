package com.bones.bson

import java.time._

import com.bones.data.custom.{AllCustomAlgebras, JavaTimeValue, LocalDateTimeData}
import com.bones.scalacheck.GenAlg.CNilGenEncoder
import com.bones.scalacheck.custom.{DefaultCustomStringValueInterpreter, DefaultScalacheckJavaUtilInterpreter, DefaultScalacheckScalaCoreInterpreter, ScalacheckJavaTimeInterpreter}
import com.bones.scalacheck.{GenAlg, ScalacheckBase}
import com.bones.schemas.Schemas._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.buffer.{ArrayBSONBuffer, ArrayReadableBuffer}

import scala.util.control.NonFatal


/** Bson dates do not support nano seconds.  need to override LocalDateTime generation in Bson
  * so that it only generates seconds
  */
object BsonScalacheck extends ScalacheckBase {

  val allInterpreters: GenAlg[AllCustomAlgebras] =
    DefaultScalacheckScalaCoreInterpreter ++
      (DefaultCustomStringValueInterpreter ++
        (OverrideJavaTimeInterpreter ++
          (DefaultScalacheckJavaUtilInterpreter ++
            CNilGenEncoder)))

  object OverrideJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter {
    override def gen[A](alg: JavaTimeValue[A]): Gen[A] = {
      alg match {
        case LocalDateTimeData(_) => {
          super.gen(alg).map((localDate: LocalDateTime) => {
            val millis = localDate.toInstant(ZoneOffset.UTC).toEpochMilli
            LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
          })
        }
        case _ => super.gen(alg)
      }
    }
  }
}

class BsonTest extends AnyFunSuite with Checkers with Matchers {

  //  implicit override val generatorDrivenConfig =
  //    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val bsonToCc = BsonValidatorInterpreter.validatorFromCustomSchema(allSupportCaseClass, com.bones.bson.custom.allValidators)

  val ccToBson = BsonEncoderInterpreter.encoderFromCustomSchema(allSupportCaseClass, com.bones.bson.custom.allEncoders)

  implicit val arb = Arbitrary(BsonScalacheck.fromCustomSchema(allSupportCaseClass, BsonScalacheck.allInterpreters))

  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {
      try {
        val buffer = new ArrayBSONBuffer()
        ccToBson(cc) match {
          case doc: BSONDocument => BSONDocument.write(doc, buffer)
          case x => fail(s"expected BSONDocument, received $x")
        }
        val bytes = buffer.array

        val readBuffer = ArrayReadableBuffer.apply(bytes)
        val doc = BSONDocument.read(readBuffer)


        val newCc = bsonToCc.apply(doc)

        newCc match {
          case Left(x) =>
            fail(s"expected success, received $x for BSON bytes ${bytes}")
          case Right(newCc2) =>
            val nullBa = Array[Byte]()

            //Arrays are only equal when they reference the same object, so let's remove them form the whole object copy
            val newCc2NoBa = newCc2.copy(ba = nullBa).copy(child = newCc2.child.copy(ba = None))
            val ccNoBA = cc.copy(ba = nullBa).copy(child = cc.child.copy(ba = None))

            newCc2NoBa == ccNoBA // && java.util.Arrays.equals(newCc2.ba, cc.ba)
        }
      } catch {
        case NonFatal(e) => { e.printStackTrace() ; false }

      }
    })

  }

}
