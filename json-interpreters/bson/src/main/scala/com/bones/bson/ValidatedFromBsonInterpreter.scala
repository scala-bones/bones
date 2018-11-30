package com.bones.bson

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.UUID

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error.{CanNotConvert, ExtractionError, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.interpreter.KvpValidateInputInterpreter
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDocument, BSONDouble, BSONLong, BSONString, BSONValue}


class ValidatedFromBsonInterpreter extends KvpValidateInputInterpreter[BSONValue] {

  type ValidatedFromJsonOption[A] =
    Option[BSONValue] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJson[A] =
    BSONValue => Either[NonEmptyList[ExtractionError], A]

  def invalidValue[T](bson: BSONValue,
                                expected: Class[T]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = bson match {
      case _: BSONBoolean  => classOf[Boolean]
      case _: BSONDouble   => classOf[Double]
      case _: BSONString   => classOf[String]
      case _: BSONArray    => classOf[Array[_]]
      case _: BSONDocument => classOf[Object]
      case _               => classOf[Any]
    }
    Left(NonEmptyList.one(WrongTypeError(expected, invalid)))
  }

  def headValue[A](in: BSONValue,
                   kv: KeyValueDefinition[A],
                   headInterpreter: Option[BSONValue] =>  Either[NonEmptyList[ExtractionError],A]
                   ): Either[NonEmptyList[ExtractionError],A] = {
    in match {
      case doc: BSONDocument =>
        val fields = doc.elements
        headInterpreter(
          fields.find(_.name == kv.key).map(_.value))
      case _ => invalidValue(in, classOf[BSONDocument])
    }

  }

  override def extractString(in: BSONValue): Either[NonEmptyList[WrongTypeError[String]], String] =
    in match {
      case BSONString(str) => Right(str)
      case x => invalidValue(x, classOf[String])
    }

  override def extractLong(in: BSONValue): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in match {
      case BSONLong(long) => Right(long)
      case x => invalidValue(x, classOf[Long])
    }

  override def extractBool(in: BSONValue): Either[NonEmptyList[WrongTypeError[Boolean]], Boolean] =
    in match{
      case BSONBoolean(bool) => Right(bool)
      case x => invalidValue(x, classOf[Boolean])
    }

  override def extractUuid(in: BSONValue): Either[NonEmptyList[ExtractionError], UUID] =
    in match {
      case BSONString(str) => stringToUuid(str)
      case x => invalidValue(x, classOf[UUID])
    }

  override def extractZonedDateTime(in: BSONValue, format: DateTimeFormatter): Either[NonEmptyList[WrongTypeError[ZonedDateTime]], ZonedDateTime] =
    in match {
      case BSONDateTime(date) =>
        val i = Instant.ofEpochSecond(date)
        Right(ZonedDateTime.ofInstant(i, ZoneOffset.UTC))
      case x => invalidValue(x, classOf[ZonedDateTime])
    }

  override def extractArray(in: BSONValue): Either[NonEmptyList[ExtractionError], Seq[BSONValue]] =
    in match {
      case BSONArray(arr) =>
        (arr.toList.map(_.toEither.leftMap(NonEmptyList.one).toValidated).sequence).toEither match {
          case Right(s) => Right(s)
          case Left(err) => Left(NonEmptyList.one(CanNotConvert(arr, classOf[Seq[_]])))
        }
      case x => invalidValue(x, classOf[Array[_]])

    }

  override def extractBigDecimal(in: BSONValue): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case BSONDouble(d) => Right(BigDecimal(d))
      case x => invalidValue(x, classOf[BigDecimal])
    }
}
