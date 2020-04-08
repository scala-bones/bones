package com.bones.validation

import com.bones.validation.ValidationDefinition.BigDecimalValidation._
import org.scalacheck.Prop._
import org.scalatestplus.scalacheck.Checkers
import com.bones.validation.ValidationDefinition.ValidValue
import org.scalatest.matchers.must.Matchers
import com.bones.Util
import cats.data.NonEmptyList
import com.bones.data.Error.CanNotConvert
import java.{util => ju}
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime

import com.bones.data.Error.RequiredValue
import com.bones.data.KvpValue
import org.scalatest.funsuite.AnyFunSuite

class UtilTest extends AnyFunSuite with Checkers with Matchers {
  val path = List("a","b")

  test("string to big decimal success") {
    Util.stringToBigDecimal("123.345", path) mustBe Right(BigDecimal("123.345"))
  }
  test("string to big decimal failure") {
    val input = "abcd"
    Util.stringToBigDecimal(input, path) match {
      case Right(success) => fail(s"unexpected success ${success}")
      case Left(err) => {
        err.head match {
          case cnc: CanNotConvert[_,_] => {
            cnc.path mustBe path
            cnc.input mustBe input
            cnc.toType mustBe classOf[BigDecimal]
            cnc.cause.isDefined mustBe true
          }
          case _ => fail(s"unexpected error: ${err}")
        }
      }
    }
  }

  test("string to uuid success") {
    val input = "4fbcdc16-3ca2-48bc-9769-bde8241a24d8"
    Util.stringToUuid(input, path) mustBe Right(ju.UUID.fromString(input))
  }

  test("string to uuid failure") {
    val input = "not uuid"
    Util.stringToUuid(input, path) match {
      case Right(success) => fail(s"unexpected success ${success}")
      case Left(err) => {
        err.head match {
          case cnc: CanNotConvert[_,_] => {
            cnc.path mustBe path
            cnc.input mustBe input
            cnc.toType mustBe classOf[ju.UUID]
            cnc.cause.isDefined mustBe true
          }
          case _ => fail(s"unexpected error: ${err}")
        }
      }
    }
  }

  test("string to local date success") {
    val input = "2019-04-01"
    val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE
    Util.stringToLocalDate(input, dateFormat, path) mustBe Right(LocalDate.of(2019,4,1))
  }

  test("string to local date failure") {
    val input = "2019-88-33"
    val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE
    Util.stringToLocalDate(input, dateFormat, path) match {
      case Right(success) => fail(s"unexpected success ${success}")
      case Left(err) => {
        err.head match {
          case cnc: CanNotConvert[_, _] => {
            cnc.path mustBe path
            cnc.input mustBe input
            cnc.toType mustBe classOf[LocalDate]
            cnc.cause.isDefined mustBe true
          }
          case _ => fail(s"unexpected error: ${err}")
        }
      }
    }
  }

  test("string to local date time success") {
    val input = "2019-04-01T18:30:00"
    val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    Util.stringToLocalDateTime(input, dateFormat, path) mustBe Right(LocalDateTime.of(2019,4,1,18,30,0))
  }

  test("string to local date time failure") {
    val input = "2019-3834"
    val dateFormat = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    Util.stringToLocalDateTime(input, dateFormat, path) match {
      case Right(success) => fail(s"unexpected success ${success}")
      case Left(err) => {
        err.head match {
          case cnc: CanNotConvert[_, _] => {
            cnc.path mustBe path
            cnc.input mustBe input
            cnc.toType mustBe classOf[LocalDateTime]
            cnc.cause.isDefined mustBe true
          }
          case _ => fail(s"unexpected error: ${err}")
        }
      }
    }
  }

  val error1 = CanNotConvert(path, "input1", classOf[LocalDate], Some(new Throwable()))
  val error2 = CanNotConvert(path, "input2", classOf[LocalDate], Some(new Throwable()))
  test("eitherMap2 accumulate error") {
    val result = Util.eitherMap2(Left(NonEmptyList.one(error1)), Left(NonEmptyList.one(error2)))( (a,b) => ??? )
    result mustBe Left(NonEmptyList(error1, List(error2)))
  }

  test("eitherMap2 error on first input") {
    val result = Util.eitherMap2(Left(NonEmptyList.one(error1)), Right("good"))( (a,b) => ???)
    result mustBe Left(NonEmptyList.one(error1))
  }

  test("eitherMap2 error on second input") {
    val result = Util.eitherMap2(Right("good"), Left(NonEmptyList.one(error2)))( (a,b) => ???)
    result mustBe Left(NonEmptyList.one(error2))
  }
  test("either map2 success") {
    val result = Util.eitherMap2(Right("good"), Right("job"))((a,b) => a+b)
    result mustBe Right("goodjob")
  }


}