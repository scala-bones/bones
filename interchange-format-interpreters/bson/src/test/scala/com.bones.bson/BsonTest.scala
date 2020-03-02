package com.bones.bson

import java.time._

import com.bones.data.{KvpValue, LocalDateTimeData}
import com.bones.scalacheck.{GenAlg, Scalacheck, ScalacheckBase}
import com.bones.schemas.Schemas._
import com.bones.validation.ValidationDefinition.LocalDateTimeValidationInstances
import org.scalacheck.Gen.Choose
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


  override val wordsGen: Gen[String] = Scalacheck.wordsGen
  override val sentencesGen: Gen[String] = Scalacheck.sentencesGen

  /** Bson DateTime doesn't support nano seconds, so we will truncate them for this test */
  val chooseMillis = new Choose[LocalDateTime] {
    override def choose(min: LocalDateTime, max: LocalDateTime): Gen[LocalDateTime] =
      Scalacheck.chooseLocalDateTime.choose(min,max).map(localDate => {
        val millis = localDate.toInstant(ZoneOffset.UTC).toEpochMilli
        LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
      })
  }

  override def valueDefinition[ALG[_], A](fgo: KvpValue[A], genAlg: GenAlg[ALG]): Gen[A] = fgo match {
    case dd: LocalDateTimeData =>
      Scalacheck.genTime(
        dd.validations,
        LocalDateTimeValidationInstances,
        LocalDateTime.of(LocalDate.ofEpochDay(Int.MinValue), LocalTime.MIN), // toEpochMilli in LocalDateTime doesn't work if the value is outside of the Int range
        LocalDateTime.of(LocalDate.ofEpochDay(Int.MaxValue), LocalTime.MAX),
        chooseMillis
      )

    case _ => super.valueDefinition(fgo, genAlg)
  }
}

class BsonTest extends AnyFunSuite with Checkers with Matchers {

  //  implicit override val generatorDrivenConfig =
  //    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  val bsonToCc = BsonValidatorInterpreter.validatorFromSchema(allSupportCaseClass)

  val ccToBson = BsonEncoderInterpreter.encoderFromSchema(allSupportCaseClass)

  implicit val arb = Arbitrary(BsonScalacheck.fromBonesSchema(allSupportCaseClass))

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
