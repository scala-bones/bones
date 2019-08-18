package com.bones.interpreter

import java.nio.charset.Charset
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error._
import com.bones.data.{KeyValueDefinition, Value}
import net.liftweb.json.JsonAST._
import com.bones.Util._
import com.bones.interpreter.KvpValidateInputInterpreter.Path
import net.liftweb.json.JsonParser.ParseException

import scala.util.Try

object ValidatedFromJObjectInterpreter {
  def isoDates = new ValidatedFromJObjectInterpreter {
    override def dateFormat: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }
}

trait ValidatedFromJObjectInterpreter
    extends KvpValidateInputInterpreter[JValue] {

  def dateFormat: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter


  def byteArrayFuncFromSchema[A](schema: Value.BonesSchema[A],
                                          charset: Charset)
    : Array[Byte] => Either[NonEmptyList[ExtractionError], A] = {
    val unmarshallFunction = fromSchema(schema)
    bytes =>
      {
        val str = new String(bytes, charset)
        try {
          val jValue = net.liftweb.json.parse(str)
          unmarshallFunction(jValue, List.empty)
        } catch {
          case ex: ParseException =>
            Left(NonEmptyList.one(ParsingError(ex.getMessage)))
        }
      }
  }

  override def headValue[A](
      in: JValue,
      kv: KeyValueDefinition[A],
      headInterpreterF: (
          Option[JValue],
          List[String]) => Either[NonEmptyList[ExtractionError], A],
      path: List[String]): Either[NonEmptyList[ExtractionError], A] = {
    in match {
      case obj: JObject =>
        headInterpreterF(obj.obj.find(_.name == kv.key).map(_.value), path)
      case _ =>
        Left(
          NonEmptyList.one(WrongTypeError(path, classOf[Object], in.getClass)))
    }
  }

  override def extractString[A](op: Value.KvpValue[A], clazz: Class[_])(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in match {
      case JString(s) => Right(s)
      case _          => Left(NonEmptyList.one(WrongTypeError(path, clazz, in.getClass)))
    }

  override def extractLong(op: Value.LongData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], Long] =
    in match {
      case JInt(i) =>
        if (i.isValidLong) Right(i.toLong)
        else
          Left(
            NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))
      case JString(s) =>
        Try { Right(s.toLong) } getOrElse { Left(
          NonEmptyList.one(
            WrongTypeError(path, classOf[Long], classOf[String])))}
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))
    }


  override def extractShort(op: Value.ShortData)(in: JValue, path: Path): Either[NonEmptyList[ExtractionError], Short] =
    in match {
      case JInt(i)    => Right(i.toShort)
      case JDouble(i) => Right(i.toShort)
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass)))
    }

  override def extractInt(op: Value.IntData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in match {
      case JInt(i)    => Right(i.toInt)
      case JDouble(d) => Right(d.toInt)
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass)))
    }

  override def extractFloat(op: Value.FloatData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in match {
      case JInt(i)    => Right(i.toFloat)
      case JDouble(d) => Right(d.toFloat)
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass)))
    }

  override def extractDouble(op: Value.DoubleData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in match {
      case JDouble(num) => Right(num)
      case JInt(i) => Right(i.toDouble)
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Double], in.getClass)))
    }

  override def extractBool(op: Value.BooleanData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in match {
      case JBool(b) => Right(b)
      case _ =>
        Left(
          NonEmptyList.one(WrongTypeError(path, classOf[Boolean], in.getClass)))
    }

  override def extractUuid(op: Value.UuidData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in match {
      case JString(s) => stringToUuid(s, path)
      case _ =>
        Left(
          NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
    }

  override def extractLocalDateTime(
      op: Value.LocalDateTimeData)(in: JValue, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    in match {
      case JString(s) => stringToLocalDateTime(s, dateFormat, path)
      case _ =>
        Left(
          NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
    }


  override def extractLocalDate(
      op: Value.LocalDateData)(in: JValue, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDate] =
    in match {
      case JString(s) => stringToLocalDate(s, dateFormat, path)
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
    }

  override def extractArray[A](op: Value.ListData[A])(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], Seq[JValue]] =
    in match {
      case JArray(s) => Right(s)
      case _ =>
        Left(
          NonEmptyList.one(
            WrongTypeError(path, classOf[Array[_]], in.getClass)))
    }

  override def extractBigDecimal(op: Value.BigDecimalData)(
      in: JValue,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case JInt(i)    => Right(BigDecimal(i))
      case JDouble(d) => Right(BigDecimal(d))
      case JString(s) =>
        Try { Right(BigDecimal(s)) } getOrElse Left(
          NonEmptyList.one(
            WrongTypeError(path, classOf[Long], classOf[String])))
      case _ =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))
    }

  override protected def invalidValue[T](
      in: JValue,
      expected: Class[T],
      path: List[String]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = in match {
      case JObject(_)       => classOf[Object]
      case JBool(_)         => classOf[Boolean]
      case JInt(_)          => classOf[Int]
      case JNothing | JNull => classOf[Nothing]
      case JArray(_)        => classOf[Array[_]]
      case JDouble(_)       => classOf[Double]
      case JString(_)       => classOf[String]
    }
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }

}
