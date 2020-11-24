package com.bones.bson

import java.time._

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  LocalDateTimeData,
  ScalaCoreValue
}
import com.bones.scalacheck.GenValue.CNilGenEncoder
import com.bones.scalacheck.values.{
  DefaultCustomStringValueInterpreter,
  DefaultScalacheckJavaTimeInterpreter,
  DefaultScalacheckJavaUtilInterpreter,
  DefaultScalacheckScalaCoreInterpreter,
  ScalacheckJavaTimeInterpreter,
  allInterpreters
}
import com.bones.scalacheck.{GenValue, ScalacheckBase}
import com.bones.schemas.Schemas._
import com.bones.bson.values._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import reactivemongo.bson.BSONDocument
import reactivemongo.bson.buffer.{ArrayBSONBuffer, ArrayReadableBuffer}
import shapeless.:+:

import scala.util.control.NonFatal

/** Bson dates do not support nano seconds.  need to override LocalDateTime generation in Bson
  * so that it only generates seconds
  */
object BsonScalacheck extends ScalacheckBase[String, DefaultValues] {

//  val allInterpreters: GenValue[DefaultValues] =
//    DefaultScalacheckScalaCoreInterpreter ++
//      (DefaultCustomStringValueInterpreter ++
//        (OverrideJavaTimeInterpreter ++
//          (DefaultScalacheckJavaUtilInterpreter ++
//            CNilGenEncoder)))
  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val allInterpreters: GenValue[DefaultValues] = {
    GenValue.merge[ScalaCoreValue, CustomStringValueCo](
      DefaultScalacheckScalaCoreInterpreter,
      GenValue.merge[CustomStringValue, JavaTimeValueCo](
        DefaultCustomStringValueInterpreter,
        GenValue.merge[JavaTimeValue, JavaUtilValueCo](
          OverrideJavaTimeInterpreter,
          GenValue
            .merge[JavaUtilValue, CNilF](DefaultScalacheckJavaUtilInterpreter, CNilGenEncoder))
      )
    )
  }

  override val genValue: GenValue[DefaultValues] = allInterpreters

  object OverrideJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter {
    override def gen[A](alg: JavaTimeValue[A]): Gen[A] = {
      alg match {
        case LocalDateTimeData(_) => {
          super
            .gen(alg)
            .map((localDate: LocalDateTime) => {
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

  val bsonToCc = defaultBsonValidatorInterpreter
    .generateValidator(allSupportCaseClass)

  val ccToBson = defaultBsonEncoderInterpreter.generateEncoder(allSupportCaseClass)

  implicit val arb = Arbitrary(BsonScalacheck.generateGen(allSupportCaseClass))

  test("scalacheck allSupport types - marshall then marshall") {
    check((cc: AllSupported) => {
      try {
        val buffer = new ArrayBSONBuffer()
        ccToBson.encode(cc) match {
          case doc: BSONDocument => BSONDocument.write(doc, buffer)
          case x                 => fail(s"expected BSONDocument, received $x")
        }
        val bytes = buffer.array

        val readBuffer = ArrayReadableBuffer.apply(bytes)
        val doc = BSONDocument.read(readBuffer)

        val newCc = bsonToCc.validate(doc)

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
        case NonFatal(e) => { e.printStackTrace(); false }

      }
    })

  }

}
