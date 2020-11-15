package com.bones.bson

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error.{CanNotConvert, ExtractionErrors, WrongTypeError}
import com.bones.data.ListData
import com.bones.interpreter.InterchangeFormatPrimitiveValidator
import reactivemongo.bson.{
  BSONArray,
  BSONBoolean,
  BSONDecimal,
  BSONDocument,
  BSONDouble,
  BSONInteger,
  BSONLong,
  BSONString,
  BSONValue
}

import scala.util.Try

object BsonPrimitiveValidator extends InterchangeFormatPrimitiveValidator[BSONValue] {

  override def extractString[ALG[_], A](op: ALG[A], typeName: String)(
    in: BSONValue,
    path: List[String]): Either[ExtractionErrors[String], String] =
    in match {
      case BSONString(str) => Right(str)
      case x               => invalidValue(x, typeName, path)
    }

  override def extractShort[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], Short] =
    in match {
      case BSONInteger(i) =>
        Try({
          i.toShort
        }).toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, "Short", "Integer", None)))
      case BSONLong(l) =>
        Try({
          l.toShort
        }).toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, "Short", "Long", None)))
      case x => invalidValue(x, "Long", path)
    }

  override def extractInt[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], Int] =
    in match {
      case BSONInteger(i) => Right(i)
      case BSONLong(l) =>
        Try({
          l.toInt
        }).toEither.left.map(_ => NonEmptyList.one(WrongTypeError(path, "Int", "Long", None)))
      case x => invalidValue(x, "Long", path)
    }

  override def extractLong[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], Long] =
    in match {
      case BSONLong(long) => Right(long)
      case BSONInteger(i) => Right(i.toLong)
      case x              => invalidValue(x, "Long", path)
    }

  override def extractBool[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], Boolean] =
    in match {
      case BSONBoolean(bool) => Right(bool)
      case x                 => invalidValue(x, "Boolean", path)
    }

  override def extractArray[ALG[_], A](op: ListData[String, ALG, A])(
    in: BSONValue,
    path: List[String]): Either[ExtractionErrors[String], Seq[BSONValue]] =
    in match {
      case BSONArray(arr) =>
        arr.toList
          .map(_.toEither.left.map(NonEmptyList.one).toValidated)
          .sequence
          .toEither match {
          case Right(s) => Right(s)
          case Left(_) =>
            Left(NonEmptyList.one(CanNotConvert(path, arr, classOf[Seq[_]], None)))
        }
      case x => invalidValue(x, op.typeName, path)

    }

  override def extractFloat[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], Float] = {
    in match {
      case BSONDouble(d) =>
        Try({ d.toFloat }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, "Float", "Double", None)))
      case dec: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(dec)
          .flatMap(d => Try { d.toFloat })
          .toEither
          .left
          .map(_ => NonEmptyList.one(WrongTypeError(path, "Float", "BSONDecimal", None)))
      case BSONInteger(i) =>
        Try({ i.toFloat }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, "Float", "Int", None)))
      case BSONLong(l) =>
        Try({ l.toFloat }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, "Float", "Long", None)))
      case x => invalidValue(x, "Float", path)
    }
  }

  override def extractDouble[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], Double] =
    in match {
      case BSONDouble(d) =>
        Right(d)
      case dec: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(dec)
          .flatMap(d => Try { d.toDouble })
          .toEither
          .left
          .map(_ => NonEmptyList.one(WrongTypeError(path, "Float", "BSONDecimal", None)))
      case BSONInteger(i) =>
        Try({ i.toDouble }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, "Double", "Int", None)))
      case BSONLong(l) =>
        Try({ l.toDouble }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, "Double", "Long", None)))
      case x => invalidValue(x, "Double", path)
    }

  override def extractBigDecimal[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], BigDecimal] =
    in match {
      case BSONDouble(d) => Right(BigDecimal(d))
      case bd: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(bd)
          .map(Right(_))
          .getOrElse(Left(NonEmptyList.one(CanNotConvert(path, in, classOf[BigDecimal], None))))
      case BSONInteger(i) => Right(BigDecimal(i))
      case BSONLong(l)    => Right(BigDecimal(l))
      case x              => invalidValue(x, "BigDecimal", path)
    }

  override def stringValue(in: BSONValue, elementName: String): Option[String] =
    in match {
      case doc: BSONDocument =>
        doc.elements.find(e => e.name == elementName).map(_.value).flatMap {
          case BSONString(str) => Some(str)
          case _               => None
        }
      case _ => None
    }

  def invalidValue[T](
    bson: BSONValue,
    typeName: String,
    path: List[String]): Left[ExtractionErrors[String], Nothing] = {
    Left(NonEmptyList.one(WrongTypeError(path, typeName, bson.getClass.getSimpleName, None)))
  }
}
