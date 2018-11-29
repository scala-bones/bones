package com.bones.interpreter

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error._
import com.bones.data.KeyValueDefinition
import net.liftweb.json.JsonAST._

import scala.util.Try

object ValidatedFromJObjectInterpreter extends KvpValidateInputInterpreter[JValue]{
  override def headValue[A](in: JValue, kv: KeyValueDefinition[A], headInterpreter: Option[JValue] => Either[NonEmptyList[ExtractionError], A]): Either[NonEmptyList[ExtractionError], A] = {
    in match {
      case obj: JObject =>
        headInterpreter(obj.obj.find(_.name == kv.key).map(_.value))
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[Object], in.getClass)))
    }
  }

  override def extractString(in: JValue): Either[NonEmptyList[WrongTypeError[String]], String] =
    in match {
      case JString(s) => Right(s)
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
    }

  override def extractLong(in: JValue): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in match {
      case JInt(i) => {
        if (i.isValidLong) Right(i.toLong)
        else Left(NonEmptyList.one(WrongTypeError(classOf[Long], in.getClass)))
      }
      case JString(s) => {
        Try { Right(s.toLong) } getOrElse(Left(NonEmptyList.one(WrongTypeError(classOf[Long], classOf[String]))))
      }
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[Long], in.getClass)))
    }


  override def extractBool(in: JValue): Either[NonEmptyList[WrongTypeError[Boolean]], Boolean] =
    in match {
      case JBool(b) => Right(b)
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[Boolean], in.getClass)))
    }

  override def extractUuid(in: JValue): Either[NonEmptyList[ExtractionError], UUID] =
    in match {
      case JString(s) => stringToUuid(s)
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
    }


  override def extractZonedDateTime(in: JValue, dateFormat: DateTimeFormatter): Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    in match {
      case JString(s) => stringToZonedDateTime(s, dateFormat)
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
    }


  override def extractArray(in: JValue): Either[NonEmptyList[ExtractionError], Seq[JValue]] =
    in match {
      case JArray(s) => Right(s)
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[Array[_]], in.getClass)))
    }

  override def extractBigDecimal(in: JValue): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case JInt(i) => Right(BigDecimal(i))
      case JDouble(d) => Right(BigDecimal(d))
      case JString(s) => {
        Try { Right(BigDecimal(s)) } getOrElse(Left(NonEmptyList.one(WrongTypeError(classOf[Long], classOf[String]))))
      }
      case _ => Left(NonEmptyList.one(WrongTypeError(classOf[Long], in.getClass)))
    }


  override protected def invalidValue[T](in: JValue, expected: Class[T]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = in match {
      case JObject(_) => classOf[Object]
      case JBool(_) => classOf[Boolean]
      case JInt(_) => classOf[Int]
      case JNothing | JNull => classOf[Nothing]
      case JArray(_) => classOf[Array[_]]
      case JDouble(_) => classOf[Double]
      case JString(_) => classOf[String]
    }
    Left(NonEmptyList.one(WrongTypeError(expected, invalid)))
  }

}
