package com.bones.circe

import java.nio.charset.Charset
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.Util._
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredData, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpValidateInputInterpreter
import io.circe.Json

object ValidatedFromCirceInterpreter {
  val isoInterpreter = new ValidatedFromCirceInterpreter {
    override def dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }
}

trait ValidatedFromCirceInterpreter extends KvpValidateInputInterpreter[Json] {

  def dateFormatter: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter

  def byteArrayFuncFromSchema[A](schema: BonesSchema[A], charset: Charset) :
  Array[Byte] => Either[NonEmptyList[ExtractionError],A] = {
    val f = fromSchema(schema)
    bytes => {
      fromByteArray(bytes, charset).flatMap(f(_,List.empty))
    }
  }


  def fromByteArray(arr: Array[Byte], charSet: Charset)
    : Either[NonEmptyList[ParsingError[Array[Byte]]], Json] = {
    val input = new String(arr, charSet)
    io.circe.parser
      .parse(input)
      .left
      .map(x => NonEmptyList.one(ParsingError(x.message)))
  }

  protected def invalidValue[T](
      json: Json,
      expected: Class[T],
      path: List[String]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = json.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }

  protected def determineError[A](
                                   in: Json,
                                   op: KvpValue[A],
                                   expectedType: Class[_],
                                   path: List[String]): NonEmptyList[ExtractionError] = {
    val error =
      if (in.isNull) RequiredData(path, op)
      else WrongTypeError(path, expectedType, in.getClass)
    NonEmptyList.one(error)
  }

  override def headValue[A](
      in: Json,
      kv: KeyValueDefinition[A],
      headInterpreter: (
          Option[Json],
          List[String]) => Either[NonEmptyList[ExtractionError], A],
      path: List[String]): Either[NonEmptyList[ExtractionError], A] =
    in.asObject match {
      case Some(jsonObj) =>
        val fields = jsonObj.toList
        headInterpreter(fields.find(_._1 == kv.key).map(_._2), path)
      case None =>
        Left(
          NonEmptyList.one(WrongTypeError(path, classOf[Object], in.getClass)))
    }

  override def extractString[A](op: KvpValue[A], clazz: Class[_])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.asString.toRight(determineError(in, op, clazz, path))


  override def extractInt(op: IntData)(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.asNumber.flatMap(_.toInt)
    .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass)))

  override def extractFloat(op: FloatData)(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.asNumber.map(_.toDouble.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Float], in.getClass)))


  override def extractDouble(op: DoubleData)(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.asNumber.map(_.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Double], in.getClass)))


  override def extractLong(op: LongData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber
      .flatMap(_.toLong)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))


  override def extractShort(op: ShortData)(in: Json, path: Path): Either[NonEmptyList[ExtractionError], Short] =
    in.asNumber.flatMap(_.toShort)
    .toRight(
      NonEmptyList.one(WrongTypeError(path, classOf[Short], in.getClass)))

  override def extractBool(op: BooleanData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in.asBoolean.toRight(determineError(in, op, classOf[Boolean], path))

  override def extractUuid(op: UuidData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in.asString
      .toRight(determineError(in, op, classOf[UUID], path))
      .flatMap(stringToUuid(_, path))

  override def extractLocalDateTime(
      op: LocalDateTimeData)(in: Json, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    in.asString
      .toRight(determineError(in, op, classOf[LocalDateTime], path))
      .flatMap(stringToLocalDateTime(_, dateFormatter, path))

  override def extractLocalDate(
      op: LocalDateData)(in: Json, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDate] =
    in.asString
    .toRight(determineError(in, op, classOf[LocalDate], path))
    .flatMap(stringToLocalDate(_, localDateFormatter, path))

  override def extractArray[A](op: ListData[A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(determineError(in, op, classOf[List[A]], path))

  override def extractBigDecimal(op: BigDecimalData)(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(determineError(in, op, classOf[BigDecimal], path))

}
