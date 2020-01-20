package com.bones.argonaut.custom

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import argonaut.Json
import cats.data.NonEmptyList
import com.bones.argonaut.ArgonautValidatorInterpreter
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, RequiredData}
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator


trait TimeValidator extends InterchangeFormatValidator[JavaTimeValue, Json] {

  val instantFormatter = DateTimeFormatter.ISO_INSTANT

  val baseInterpreter = ArgonautValidatorInterpreter.isoInterpreter

  private def errorHandleTimeParsing[A](path: List[String], f: String => A, input: String): Either[NonEmptyList[Error.ExtractionError], A] =
    try {
      Right(f(input))
    } catch {
      case ex: DateTimeParseException => Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime])))
      case ex: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime])))

    }

  private def parseTime[A](alg: JavaTimeValue[A], f: String => A): (Option[Json], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (jsonOpt:Option[Json], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseInterpreter
            .extractString(Right(alg), classOf[Duration])(json, path)
            .flatMap(result => errorHandleTimeParsing(path, f, result))
        case None =>
          Left(NonEmptyList.one(RequiredData(path, Right(alg))))
      }
    }
  }

  override def validate[A](alg: JavaTimeValue[A]):
  (Option[Json], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    alg match {
      case DurationData(_) => parseTime(alg, input => Duration.parse(input))
      case InstantData(_) => parseTime(alg, input => Instant.parse(input))
      case MonthData(_) => parseTime(alg, input => Month.valueOf(input))
      case PeriodData(_) => parseTime(alg, input => Period.parse(input))
      case ZoneIdData(_) => parseTime(alg, input => ZoneId.of(input))
      case ZoneOffsetData(_) => parseTime(alg, input => ZoneOffset.of(input))
    }
  }
}
