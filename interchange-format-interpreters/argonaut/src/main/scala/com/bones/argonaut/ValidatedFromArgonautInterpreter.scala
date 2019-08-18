package com.bones.argonaut

import java.nio.charset.Charset
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import argonaut.Argonaut._
import argonaut._
import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredData, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpValidateInputInterpreter
import com.bones.Util._

import scala.util.control.NonFatal

object ValidatedFromArgonautInterpreter {
  val isoInterpreter = new ValidatedFromArgonautInterpreter {
    override def dateFormat: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }
}

trait ValidatedFromArgonautInterpreter extends KvpValidateInputInterpreter[Json]{
  def dateFormat: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter

  def byteArrayFuncFromSchema[A](schema: BonesSchema[A],
                                          charset: Charset)
    : Array[Byte] => Either[NonEmptyList[ExtractionError], A] = {
    val fromSchemaFunction = fromSchema(schema)
    bytes =>
      {
        try {
          val str = new String(bytes, charset)
          Parse
            .parse(str)
            .left
            .map(str => NonEmptyList.one(ParsingError(str)))
            .flatMap(fromSchemaFunction(_, List.empty))
        } catch {
          case NonFatal(ex) =>
            Left(NonEmptyList.one(ParsingError(ex.getMessage, Some(ex))))
        }
      }
  }

  def fromByteArray(arr: Array[Byte],
                    charset: Charset): Either[ExtractionError, Json] =
    Parse.parse(new String(arr, charset)).left.map(err => ParsingError(err))

  override def headValue[A](
      in: Json,
      kv: KeyValueDefinition[A],
      headInterpreter: (
          Option[Json],
          List[String]) => Either[NonEmptyList[ExtractionError], A],
      path: List[String]): Either[NonEmptyList[ExtractionError], A] = {

    in.obj
      .toRight[NonEmptyList[ExtractionError]](
        NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass)))
      .flatMap(
        _.toList
          .find(f => f._1 === kv.key)
          .toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(path, kv.op))))
      .flatMap(j => headInterpreter.apply(Some(j._2), path))
  }

  override def extractString[A](op: KvpValue[A], clazz: Class[_])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.string.toRight(
      NonEmptyList.one(WrongTypeError(path, clazz, in.getClass)))


  override def extractShort(op: ShortData)(in: Json, path: Path):
      Either[NonEmptyList[ExtractionError], Short] =
    in.number
    .flatMap(n => n.toShort)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Short], in.getClass)))

  override def extractInt(op: IntData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.number
      .flatMap(n => n.toInt)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass)))

  override def extractLong(op: LongData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Long] =
    in.number
      .flatMap(_.toLong)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))

  override def extractBool(op: BooleanData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], JsonBoolean] =
    in.bool.toRight(
      NonEmptyList.one(WrongTypeError(path, classOf[Boolean], in.getClass)))

  override def extractUuid(op: UuidData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in.string
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
      .flatMap(stringToUuid(_, path))

  override def extractLocalDateTime(
      op: LocalDateTimeData)(in: Json, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    in.string
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
      .flatMap(stringToLocalDateTime(_, dateFormat, path))

  def extractLocalDate(dataData: LocalDateData)(in: Json, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDate] =
    in.string
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[String], in.getClass)))
    .flatMap(stringToLocalDate(_,localDateFormatter, path))


  override def extractArray[A](op: ListData[A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.array.toRight(
      NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass)))

  override def extractFloat(op: FloatData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.number
      .flatMap(n => n.toFloat)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Byte], in.getClass)))

  override def extractDouble(op: DoubleData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.number
      .flatMap(n => n.toDouble)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Byte], in.getClass)))

  override def extractBigDecimal(op: BigDecimalData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.number
      .map(_.toBigDecimal)
      .toRight(NonEmptyList.one(
        WrongTypeError(path, classOf[BigDecimal], in.getClass)))

  override protected def invalidValue[T](
      in: Json,
      expected: Class[T],
      path: List[String]): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = in.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }
}
