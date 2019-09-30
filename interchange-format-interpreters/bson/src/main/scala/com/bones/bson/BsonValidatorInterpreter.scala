package com.bones.bson

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error._
import com.bones.data.{KeyValueDefinition, Value}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.Util._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.Path
import reactivemongo.bson.buffer.ArrayReadableBuffer
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDecimal, BSONDocument, BSONDouble, BSONInteger, BSONLong, BSONNull, BSONString, BSONValue}

import scala.util.Try

/**
  * Module responsible for validating data from BSON and convering to Values.
  */
object BsonValidatorInterpreter
  extends KvpInterchangeFormatValidatorInterpreter[BSONValue] {

  def fromByteArray(
                     arr: Array[Byte]): Either[NonEmptyList[ExtractionError], BSONValue] = {
    val buffer = ArrayReadableBuffer(arr)
    Try {
      BSONDocument.read(buffer)
    }.toEither.left.map(err => NonEmptyList.one(ParsingError(err.getMessage)))
  }


  override def isEmpty(json: BSONValue): Boolean = json match {
    case BSONNull => true
    case _ => false
  }

  type ValidatedFromJsonOption[A] =
    Option[BSONValue] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJson[A] =
    BSONValue => Either[NonEmptyList[ExtractionError], A]

  override protected def invalidValue[T](
                                          bson: BSONValue,
                                          expected: Class[T],
                                          path: List[String]): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = bson match {
      case _: BSONBoolean => classOf[Boolean]
      case _: BSONDouble => classOf[Double]
      case _: BSONString => classOf[String]
      case _: BSONArray => classOf[Array[_]]
      case _: BSONDocument => classOf[Object]
      case _ => classOf[Any]
    }
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }

  override def headValue[A](
                             in: BSONValue,
                             kv: KeyValueDefinition[A],
                             headInterpreter: (
                               Option[BSONValue],
                                 List[String]) => Either[NonEmptyList[ExtractionError], A],
                             path: List[String]): Either[NonEmptyList[ExtractionError], A] = {
    in match {
      case doc: BSONDocument =>
        val fields = doc.elements
        headInterpreter(fields.find(_.name == kv.key).map(_.value), path)
      case _ => invalidValue(in, classOf[BSONDocument], path)
    }

  }

  override def extractString[A](op: Value.KvpValue[A],
                                clazz: Class[_])(
                                 in: BSONValue,
                                 path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in match {
      case BSONString(str) => Right(str)
      case x => invalidValue(x, clazz, path)
    }


  override def extractShort(op: Value.ShortData)(in: BSONValue, path: Path): Either[NonEmptyList[ExtractionError], Short] =
    in match {
      case BSONInteger(i) =>
        Try({
          i.toShort
        }).toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Short], classOf[Integer])))
      case BSONLong(l) =>
        Try({
          l.toShort
        }).toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Short], classOf[Long])))
      case x => invalidValue(x, classOf[Long], path)
    }

  override def extractInt(op: Value.IntData)(in: BSONValue, path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in match {
      case BSONInteger(i) => Right(i)
      case BSONLong(l) =>
        Try({
          l.toInt
        }).toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Byte], classOf[Long])))
      case x => invalidValue(x, classOf[Long], path)
    }


  override def extractLong(op: Value.LongData)(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Long] =
    in match {
      case BSONLong(long) => Right(long)
      case BSONInteger(i) => Right(i.toLong)
      case x => invalidValue(x, classOf[Long], path)
    }

  override def extractBool(op: Value.BooleanData)(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in match {
      case BSONBoolean(bool) => Right(bool)
      case x => invalidValue(x, classOf[Boolean], path)
    }

  override def extractUuid(op: Value.UuidData)(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in match {
      case BSONString(str) => stringToUuid(str, path)
      case x => invalidValue(x, classOf[UUID], path)
    }

  override def extractLocalDateTime(op: Value.LocalDateTimeData)(in: BSONValue, path: List[String])
  : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    in match {
      case BSONDateTime(date) =>
        val i = Instant.ofEpochMilli(date)
        Right(LocalDateTime.ofInstant(i, ZoneOffset.UTC))
      case x => invalidValue(x, classOf[BSONDateTime], path)
    }


  override def extractLocalDate(op: Value.LocalDateData)(in: BSONValue, path: List[String])
  : Either[NonEmptyList[ExtractionError], LocalDate] =
    in match {
      case BSONDateTime(date) =>
        Right(LocalDate.ofEpochDay(date))
      case x => invalidValue(x, classOf[BSONDateTime], path)
    }

  override def extractArray[A](op: Value.ListData[A])(in: BSONValue,
                                                      path: List[String])
  : Either[NonEmptyList[ExtractionError], Seq[BSONValue]] =
    in match {
      case BSONArray(arr) =>
        arr.toList
          .map(_.toEither.leftMap(NonEmptyList.one).toValidated)
          .sequence.toEither match {
          case Right(s) => Right(s)
          case Left(_) =>
            Left(NonEmptyList.one(CanNotConvert(path, arr, classOf[Seq[_]])))
        }
      case x => invalidValue(x, classOf[Array[_]], path)

    }


  override def extractFloat(op: Value.FloatData)(in: BSONValue, path: List[String]): Either[NonEmptyList[ExtractionError], Float] = {
    in match {
      case BSONDouble(d) =>
        Try({d.toFloat})
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[Double])))
      case dec:BSONDecimal =>
        BSONDecimal.toBigDecimal(dec)
          .flatMap(d => Try {d.toFloat} )
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[BSONDecimal])))
      case BSONInteger(i) =>
        Try({i.toFloat})
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[Int])))
      case BSONLong(l) =>
        Try({l.toFloat})
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[Long])))
      case x => invalidValue(x, classOf[Float], path)
    }
  }

  override def extractDouble(op: Value.DoubleData)(in: BSONValue, path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in match {
      case BSONDouble(d) =>
        Right(d)
      case dec:BSONDecimal =>
        BSONDecimal.toBigDecimal(dec)
          .flatMap(d => Try {d.toDouble} )
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[BSONDecimal])))
      case BSONInteger(i) =>
        Try({i.toDouble})
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Double], classOf[Int])))
      case BSONLong(l) =>
        Try({l.toDouble})
          .toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, classOf[Double], classOf[Long])))
      case x => invalidValue(x, classOf[Float], path)
    }



  override def extractBigDecimal(op: Value.BigDecimalData)(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case BSONDouble(d) => Right(BigDecimal(d))
      case bd: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(bd)
          .map(Right(_))
          .getOrElse(Left(
            NonEmptyList.one(CanNotConvert(path, in, classOf[BigDecimal]))))
      case BSONInteger(i) => Right(BigDecimal(i))
      case BSONLong(l) => Right(BigDecimal(l))
      case x => invalidValue(x, classOf[BigDecimal], path)
    }
}
