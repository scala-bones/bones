package com.bones.circe

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.interpreter.KvpValidateInputInterpreter
import io.circe.Json

object ValidatedFromCirceInterpreter extends KvpValidateInputInterpreter[Json] {

  protected def invalidValue[T](json: Json, expected: Class[T]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = json.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(expected, invalid)))
  }

  override def headValue[A](in: Json, kv: KeyValueDefinition[A], headInterpreter: Option[Json] => Either[NonEmptyList[ExtractionError], A]): Either[NonEmptyList[ExtractionError], A] = {
    in.asObject match {
      case Some(jsonObj) =>
        val fields = jsonObj.toList
        headInterpreter(fields.find(_._1 == kv.key).map(_._2))
      case None => Left(NonEmptyList.one(WrongTypeError(classOf[Object], in.getClass)))
    }
  }

  override def extractString(in: Json): Either[NonEmptyList[WrongTypeError[String]], String] =
    in.asString.toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))

  override def extractLong(in: Json): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber.flatMap(_.toLong).toRight(NonEmptyList.one(WrongTypeError(classOf[Long], in.getClass)))


  override def extractBool(in: Json): Either[NonEmptyList[WrongTypeError[Boolean]], Boolean] =
    in.asBoolean.toRight(NonEmptyList.one(WrongTypeError(classOf[Boolean], in.getClass)))


  override def extractUuid(in: Json): Either[NonEmptyList[ExtractionError], UUID] =
    in.asString
      .toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
      .flatMap(stringToUuid)


  override def extractZonedDateTime(in: Json, dateFormat: DateTimeFormatter): Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    in.asString
      .toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
      .flatMap(stringToZonedDateTime(_,dateFormat))


  override def extractArray(in: Json): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))


  override def extractBigDecimal(in: Json): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))


}

