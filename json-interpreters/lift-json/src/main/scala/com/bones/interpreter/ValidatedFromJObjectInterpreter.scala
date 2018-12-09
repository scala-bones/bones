package com.bones.interpreter

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error._
import com.bones.data.{KeyValueDefinition, Value}
import net.liftweb.json.JsonAST._

import scala.util.Try

object ValidatedFromJObjectInterpreter extends KvpValidateInputInterpreter[JValue]{


  override def headValue[A](in: JValue,
                            kv: KeyValueDefinition[A],
                            headInterpreter: (Option[JValue], Vector[String]) => Either[NonEmptyList[ExtractionError], A],
                            path: Vector[String]): Either[NonEmptyList[ExtractionError], A] = {
    in match {
      case obj: JObject =>
        headInterpreter(obj.obj.find(_.name == kv.key).map(_.value), path)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[Object], in.getClass)))
    }
  }



  override def extractString[A](op: Value.ValueDefinitionOp[A], clazz: Class[_])(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], String] =
    in match {
      case JString(s) => Right(s)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, clazz, in.getClass)))
    }

  override def extractLong(op: Value.LongData)(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], Long] =
    in match {
      case JInt(i) => {
        if (i.isValidLong) Right(i.toLong)
        else Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))
      }
      case JString(s) => {
        Try { Right(s.toLong) } getOrElse(Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], classOf[String]))))
      }
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))
    }

  override def extractBool(op: Value.BooleanData)(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in match {
      case JBool(b) => Right(b)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[Boolean], in.getClass)))
    }


  override def extractUuid(op: Value.UuidData)(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in match {
      case JString(s) => stringToUuid(s,path)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
    }


  override def extractZonedDateTime(dateFormat: DateTimeFormatter, op: Value.DateTimeData)(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    in match {
      case JString(s) => stringToZonedDateTime(s, dateFormat, path)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
    }


  override def extractArray[A](op: Value.ListData[A])(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], Seq[JValue]] =
    in match {
      case JArray(s) => Right(s)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass)))
    }


  override def extractXMapArray[A](op: Value.XMapListData[A])(in: JValue, path: Vector[String]): Either[NonEmptyList[ExtractionError], Seq[JValue]] =
    in match {
      case JArray(s) => Right(s)
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass)))
    }


  override def extractBigDecimal(op: Value.BigDecimalData)(in: JValue, path: Vector[String]):
    Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case JInt(i) => Right(BigDecimal(i))
      case JDouble(d) => Right(BigDecimal(d))
      case JString(s) => {
        Try { Right(BigDecimal(s)) } getOrElse Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], classOf[String])))
      }
      case _ => Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))
    }


  override protected def invalidValue[T](in: JValue, expected: Class[T], path: Vector[String]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = in match {
      case JObject(_) => classOf[Object]
      case JBool(_) => classOf[Boolean]
      case JInt(_) => classOf[Int]
      case JNothing | JNull => classOf[Nothing]
      case JArray(_) => classOf[Array[_]]
      case JDouble(_) => classOf[Double]
      case JString(_) => classOf[String]
    }
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }

}
