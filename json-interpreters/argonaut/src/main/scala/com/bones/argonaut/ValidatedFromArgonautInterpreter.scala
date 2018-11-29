package com.bones.argonaut

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value._
import cats.implicits._
import shapeless.{HList, HNil, Nat}
import com.bones.validation.{ValidationUtil => vu}
import argonaut._
import Argonaut._
import com.bones.data.KeyValueDefinition
import com.bones.interpreter.KvpValidateInputInterpreter

import scala.util.control.NonFatal

object ValidatedFromArgonautInterpreter extends KvpValidateInputInterpreter[Json] {
  override def headValue[A](in: Json, kv: KeyValueDefinition[A], headInterpreter: Option[Json] => Either[NonEmptyList[ExtractionError], A]): Either[NonEmptyList[ExtractionError], A] = {
    in.array
      .toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(WrongTypeError(classOf[Array[_]], in.getClass)))
      .flatMap(_.find(j => j.name === kv.key).toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(RequiredData(kv.op))))
      .flatMap(j => headInterpreter.apply(Some(j)))
  }


  override def extractString(in: Json): Either[NonEmptyList[WrongTypeError[String]], String] =
    in.string.toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))

  override def extractLong(in: Json): Either[NonEmptyList[WrongTypeError[Long]], Long] =
      in.number.flatMap(_.toLong).toRight(NonEmptyList.one(WrongTypeError(classOf[Long], in.getClass)))

  override def extractBool(in: Json): Either[NonEmptyList[WrongTypeError[JsonBoolean]], JsonBoolean] =
    in.bool.toRight(NonEmptyList.one(WrongTypeError(classOf[Boolean], in.getClass)))

  override def extractUuid(in: Json): Either[NonEmptyList[ExtractionError], UUID] =
    in.string
      .toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
      .flatMap(stringToUuid)

  override def extractZonedDateTime(in: Json, dateFormat: DateTimeFormatter): Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    in.string
      .toRight(NonEmptyList.one(WrongTypeError(classOf[String], in.getClass)))
      .flatMap(stringToZonedDateTime(_,dateFormat))


  override def extractArray(in: Json): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.array.toRight(NonEmptyList.one(WrongTypeError(classOf[Array[_]],in.getClass)))


  override def extractBigDecimal(in: Json): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.number.map(_.toBigDecimal).toRight(NonEmptyList.one(WrongTypeError(classOf[BigDecimal], in.getClass)))

  override protected def invalidValue[T](in: Json, expected: Class[T]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = in.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(expected, invalid)))
  }
}
