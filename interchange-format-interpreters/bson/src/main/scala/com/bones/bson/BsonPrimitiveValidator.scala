package com.bones.bson

import com.bones.Util
import com.bones.data.Error.{CanNotConvert, ExtractionErrors, WrongTypeError}
import com.bones.interpreter.validator.{InterchangeFormatPrimitiveValidator, Validator}
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

  override def extractString[ALG[_], A](
    typeName: String): Validator[String, ALG, String, BSONValue] = {
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONString(str) => Right(str)
        case x               => invalidValue(x, typeName, path)
      }
  }

  override def extractShort[ALG[_], A]: Validator[String, ALG, Short, BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONInteger(i) =>
          Try({
            i.toShort
          }).toEither.left.map(_ => List(WrongTypeError(path, "Short", "Integer", None)))
        case BSONLong(l) =>
          Try({
            l.toShort
          }).toEither.left.map(_ => List(WrongTypeError(path, "Short", "Long", None)))
        case x => invalidValue(x, "Long", path)
    }

  override def extractInt[ALG[_], A]: Validator[String, ALG, Int, BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONInteger(i) => Right(i)
        case BSONLong(l) =>
          Try({
            l.toInt
          }).toEither.left.map(_ => List(WrongTypeError(path, "Int", "Long", None)))
        case x => invalidValue(x, "Long", path)
    }

  override def extractLong[ALG[_], A]: Validator[String, ALG, Long, BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONLong(long) => Right(long)
        case BSONInteger(i) => Right(i.toLong)
        case x              => invalidValue(x, "Long", path)
    }

  override def extractBool[ALG[_], A]: Validator[String, ALG, Boolean, BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONBoolean(bool) => Right(bool)
        case x                 => invalidValue(x, "Boolean", path)
    }

  override def extractArray[ALG[_], A](
    typeName: String): Validator[String, ALG, Seq[BSONValue], BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONArray(arr) =>
          Util.sequence(
            arr.toList
              .map(_.toEither.left.map(List(_)))) match {
            case Right(s) => Right(s)
            case Left(_) =>
              Left(List(CanNotConvert(path, arr, classOf[Seq[_]], None)))
          }
        case x => invalidValue(x, typeName, path)

    }

  override def extractFloat[ALG[_], A]: Validator[String, ALG, Float, BSONValue] = {
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONDouble(d) =>
          Try({ d.toFloat }).toEither.left.map(_ =>
            List(WrongTypeError(path, "Float", "Double", None)))
        case dec: BSONDecimal =>
          BSONDecimal
            .toBigDecimal(dec)
            .flatMap(d => Try { d.toFloat })
            .toEither
            .left
            .map(_ => List(WrongTypeError(path, "Float", "BSONDecimal", None)))
        case BSONInteger(i) =>
          Try({ i.toFloat }).toEither.left.map(_ =>
            List(WrongTypeError(path, "Float", "Int", None)))
        case BSONLong(l) =>
          Try({ l.toFloat }).toEither.left.map(_ =>
            List(WrongTypeError(path, "Float", "Long", None)))
        case x => invalidValue(x, "Float", path)
      }
  }

  override def extractDouble[ALG[_], A]: Validator[String, ALG, Double, BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONDouble(d) =>
          Right(d)
        case dec: BSONDecimal =>
          BSONDecimal
            .toBigDecimal(dec)
            .flatMap(d => Try { d.toDouble })
            .toEither
            .left
            .map(_ => List(WrongTypeError(path, "Float", "BSONDecimal", None)))
        case BSONInteger(i) =>
          Try({ i.toDouble }).toEither.left.map(_ =>
            List(WrongTypeError(path, "Double", "Int", None)))
        case BSONLong(l) =>
          Try({ l.toDouble }).toEither.left.map(_ =>
            List(WrongTypeError(path, "Double", "Long", None)))
        case x => invalidValue(x, "Double", path)
    }

  override def extractBigDecimal[ALG[_], A]: Validator[String, ALG, BigDecimal, BSONValue] =
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONDouble(d) => Right(BigDecimal(d))
        case bd: BSONDecimal =>
          BSONDecimal
            .toBigDecimal(bd)
            .map(Right(_))
            .getOrElse(Left(List(CanNotConvert(path, in, classOf[BigDecimal], None))))
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
    Left(List(WrongTypeError(path, typeName, bson.getClass.getSimpleName, None)))
  }
}
