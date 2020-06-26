package com.bones.bson

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error._
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import reactivemongo.bson.buffer.ArrayReadableBuffer
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDecimal, BSONDocument, BSONDouble, BSONInteger, BSONLong, BSONNull, BSONString, BSONValue}

import scala.util.Try

/**
  * Module responsible for validating data from BSON and convering to Values.
  */
object BsonValidatorInterpreter extends KvpInterchangeFormatValidatorInterpreter[BSONValue] {


  /** An additional string in the serialized format which states the coproduct type.
    * TODO:  refactor this interpreter so this property can be overwritten. */
  override val coproductTypeKey: String = "type"

  trait BsonValidator[ALG[_]] extends InterchangeFormatValidator[ALG, BSONValue]

  def fromByteArray(arr: Array[Byte]): Either[NonEmptyList[ExtractionError], BSONValue] = {
    val buffer = ArrayReadableBuffer(arr)
    Try {
      BSONDocument.read(buffer)
    }.toEither.left.map(err => NonEmptyList.one(ParsingError(err.getMessage)))
  }

  override def isEmpty(json: BSONValue): Boolean = json match {
    case BSONNull => true
    case _        => false
  }

  type ValidatedFromJsonOption[A] =
    Option[BSONValue] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJson[A] =
    BSONValue => Either[NonEmptyList[ExtractionError], A]

  override def invalidValue[T](
    bson: BSONValue,
    expected: Class[T],
    path: List[String]): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = bson match {
      case _: BSONBoolean  => classOf[Boolean]
      case _: BSONDouble   => classOf[Double]
      case _: BSONString   => classOf[String]
      case _: BSONArray    => classOf[Array[_]]
      case _: BSONDocument => classOf[Object]
      case _               => classOf[Any]
    }
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid, None)))
  }

  override def headValue[ALG[_], A](
    in: BSONValue,
    kv: KeyValueDefinition[ALG, A],
    headInterpreter: (Option[BSONValue], List[String]) => Either[NonEmptyList[ExtractionError], A],
    path: List[String]): Either[NonEmptyList[ExtractionError], A] = {
    in match {
      case doc: BSONDocument =>
        val fields = doc.elements
        headInterpreter(fields.find(_.name == kv.key).map(_.value), path)
      case _ => invalidValue(in, classOf[BSONDocument], path)
    }

  }

  override def extractString[ALG[_], A](op: ALG[A], clazz: Class[_])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in match {
      case BSONString(str) => Right(str)
      case x               => invalidValue(x, clazz, path)
    }

  override def extractShort[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Short] =
    in match {
      case BSONInteger(i) =>
        Try({
          i.toShort
        }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Short], classOf[Integer], None)))
      case BSONLong(l) =>
        Try({
          l.toShort
        }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Short], classOf[Long], None)))
      case x => invalidValue(x, classOf[Long], path)
    }

  override def extractInt[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in match {
      case BSONInteger(i) => Right(i)
      case BSONLong(l) =>
        Try({
          l.toInt
        }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Byte], classOf[Long], None)))
      case x => invalidValue(x, classOf[Long], path)
    }

  override def extractLong[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Long] =
    in match {
      case BSONLong(long) => Right(long)
      case BSONInteger(i) => Right(i.toLong)
      case x              => invalidValue(x, classOf[Long], path)
    }

  override def extractBool[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in match {
      case BSONBoolean(bool) => Right(bool)
      case x                 => invalidValue(x, classOf[Boolean], path)
    }



  override def extractArray[ALG[_], A](op: ListData[ALG, A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Seq[BSONValue]] =
    in match {
      case BSONArray(arr) =>
        arr.toList
          .map(_.toEither.leftMap(NonEmptyList.one).toValidated)
          .sequence
          .toEither match {
          case Right(s) => Right(s)
          case Left(_) =>
            Left(NonEmptyList.one(CanNotConvert(path, arr, classOf[Seq[_]], None)))
        }
      case x => invalidValue(x, classOf[Array[_]], path)

    }

  override def extractFloat[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Float] = {
    in match {
      case BSONDouble(d) =>
        Try({ d.toFloat }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[Double], None)))
      case dec: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(dec)
          .flatMap(d => Try { d.toFloat })
          .toEither
          .left
          .map(_ =>
            NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[BSONDecimal], None)))
      case BSONInteger(i) =>
        Try({ i.toFloat }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[Int], None)))
      case BSONLong(l) =>
        Try({ l.toFloat }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[Long], None)))
      case x => invalidValue(x, classOf[Float], path)
    }
  }

  override def extractDouble[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in match {
      case BSONDouble(d) =>
        Right(d)
      case dec: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(dec)
          .flatMap(d => Try { d.toDouble })
          .toEither
          .left
          .map(_ =>
            NonEmptyList.one(WrongTypeError(path, classOf[Float], classOf[BSONDecimal], None)))
      case BSONInteger(i) =>
        Try({ i.toDouble }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Double], classOf[Int], None)))
      case BSONLong(l) =>
        Try({ l.toDouble }).toEither.left.map(_ =>
          NonEmptyList.one(WrongTypeError(path, classOf[Double], classOf[Long], None)))
      case x => invalidValue(x, classOf[Float], path)
    }

  override def extractBigDecimal[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case BSONDouble(d) => Right(BigDecimal(d))
      case bd: BSONDecimal =>
        BSONDecimal
          .toBigDecimal(bd)
          .map(Right(_))
          .getOrElse(Left(NonEmptyList.one(CanNotConvert(path, in, classOf[BigDecimal], None))))
      case BSONInteger(i) => Right(BigDecimal(i))
      case BSONLong(l)    => Right(BigDecimal(l))
      case x              => invalidValue(x, classOf[BigDecimal], path)
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
}
