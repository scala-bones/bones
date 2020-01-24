package com.bones.circe

import java.nio.charset.Charset
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.Util._
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredData, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data.KvpValue.Path
import com.bones.data._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.{InterchangeFormatValidator, NoAlgebraValidator}
import io.circe.Json

object CirceValidatorInterpreter {

  /**
    * An implementation of the [CirceValidatorInterpreter] assuming date/datetime formats are in iso format.
    */
  val isoInterpreter: CirceValidatorInterpreter = new CirceValidatorInterpreter {
    override def dateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override def localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  val noAlgebraInterpreter = NoAlgebraValidator[Json]()
}

/**
  * Module responsible for converting circe JSON input into values with validation checks.
  * See [KvpInterchangeFormatValidatorInterpreter.fromSchema] for the entry point.
  */
trait CirceValidatorInterpreter extends KvpInterchangeFormatValidatorInterpreter[Json] {

  def dateFormatter: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter
  def localTimeFormatter: DateTimeFormatter


  override def isEmpty(json: Json): Boolean = json.isNull

  def byteArrayFuncFromSchema[ALG[_], A]
    (
      schema: BonesSchema[ALG, A],
      charset: Charset,
      validatorInterpreter: InterchangeFormatValidator[ALG,Json]
    ) : Array[Byte] => Either[NonEmptyList[ExtractionError],A] = {
      val f = fromCustomSchema(schema, validatorInterpreter)
      bytes => {
        fromByteArray(bytes, charset).flatMap(f(_))
      }
    }

  def fromByteArray(arr: Array[Byte], charSet: Charset)
    : Either[NonEmptyList[ParsingError], Json] = {
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

  protected def determineError[ALG[_], A](
                                   in: Json,
                                   op: CoproductDataDefinition[ALG, A],
                                   expectedType: Class[_],
                                   path: List[String]): NonEmptyList[ExtractionError] = {
    val error =
      if (in.isNull) RequiredData(path, op)
      else WrongTypeError(path, expectedType, in.getClass)
    NonEmptyList.one(error)
  }

  override def headValue[ALG[_], A](
      in: Json,
      kv: KeyValueDefinition[ALG, A],
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

  override def extractString[ALG[_], A](op: CoproductDataDefinition[ALG, A], clazz: Class[_])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.asString.toRight(determineError(in, op, clazz, path))


  override def extractInt[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.asNumber.flatMap(_.toInt)
    .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass)))

  override def extractFloat[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.asNumber.map(_.toDouble.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Float], in.getClass)))


  override def extractDouble[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.asNumber.map(_.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Double], in.getClass)))


  override def extractLong[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber
      .flatMap(_.toLong)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass)))


  override def extractShort[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: Path): Either[NonEmptyList[ExtractionError], Short] =
    in.asNumber.flatMap(_.toShort)
    .toRight(
      NonEmptyList.one(WrongTypeError(path, classOf[Short], in.getClass)))

  override def extractBool[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in.asBoolean.toRight(determineError(in, op, classOf[Boolean], path))

  override def extractUuid[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in.asString
      .toRight(determineError(in, op, classOf[UUID], path))
      .flatMap(stringToUuid(_, path))

  override def extractLocalDateTime[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    in.asString
      .toRight(determineError(in, op, classOf[LocalDateTime], path))
      .flatMap(stringToLocalDateTime(_, dateFormatter, path))

  override def extractLocalDate[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDate] =
    in.asString
    .toRight(determineError(in, op, classOf[LocalDate], path))
    .flatMap(stringToLocalDate(_, localDateFormatter, path))


  override protected def extractLocalTime[ALG[_], A](op: CoproductDataDefinition[ALG, A])(in: Json, path: Path)
    : Either[NonEmptyList[ExtractionError], LocalTime] =
    in.asString
    .toRight(determineError(in, op, classOf[LocalTime], path))
    .flatMap(stringToLocalTime(_, localTimeFormatter, path))

  override def extractArray[ALG[_], A](op: ListData[ALG, A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(determineError(in, Left(op), classOf[List[A]], path))

  override def extractBigDecimal[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
      in: Json,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(determineError(in, op, classOf[BigDecimal], path))

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.asObject
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.asString
    } yield value

}
