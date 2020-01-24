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
import com.bones.data.Error.ExtractionError

/** Time validator, uses the ISO Specific formatters */
trait IsoTimeValidator extends TimeValidator {
  val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
  val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
  val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

}

trait TimeValidator extends InterchangeFormatValidator[JavaTimeValue, Json] {

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  val baseInterpreter = ArgonautValidatorInterpreter.isoInterpreter

  private def errorHandleTimeParsing[A](path: List[String], f: String => A, input: String): Either[NonEmptyList[Error.ExtractionError], A] =
    try {
      Right(f(input))
    } catch {
      case ex: DateTimeParseException => Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime])))
      case ex: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime])))
    }

  private def parseTime[A](alg: JavaTimeValue[A], clazz: Class[A], f: String => A): (Option[Json], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (jsonOpt:Option[Json], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseInterpreter
            .extractString(Right(alg), clazz)(json, path)
            .flatMap(result => errorHandleTimeParsing(path, f, result))
        case None =>
          Left(NonEmptyList.one(RequiredData(path, Right(alg))))
      }
    }
  }

  private def parseYear[A](alg: JavaTimeValue[A]): (Option[Json], List[String]) => Either[NonEmptyList[Error.ExtractionError], Year] = 
    (jsonOpt:Option[Json], path: List[String]) => {
      jsonOpt match {
      case Some(json) =>
        baseInterpreter
          .extractInt(Right(alg))(json, path)
          .map(result => Year.of(result))
      case None =>
        Left(NonEmptyList.one(RequiredData(path, Right(alg))))
    }
  }

  override def validate[A](alg: JavaTimeValue[A]):
  (Option[Json], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    alg match {
      case DateTimeExceptionData(_) => parseTime(alg, classOf[DateTimeException], input => new DateTimeException(input))
      case DayOfWeekData(_) => parseTime(alg, classOf[DayOfWeek], DayOfWeek.valueOf)
      case DurationData(_) => parseTime(alg, classOf[Duration], Duration.parse)
      case InstantData(_) => parseTime(alg, classOf[Instant], input => Instant.from(instantFormatter.parse(input)))
      case MonthData(_) => parseTime(alg, classOf[Month], Month.valueOf)
      case MonthDayData(_) => parseTime(alg, classOf[MonthDay], MonthDay.parse)
      case OffsetDateTimeData(_) => parseTime(alg, classOf[OffsetDateTime], OffsetDateTime.parse(_, offsetDateTimeFormatter))
      case OffsetTimeData(_) => parseTime(alg, classOf[OffsetTime], OffsetTime.parse(_, offsetTimeFormatter))
      case PeriodData(_) => parseTime(alg, classOf[Period], Period.parse)
      case YearData(_) => parseYear(alg)
      case YearMonthData(_) => parseTime(alg, classOf[YearMonth], YearMonth.parse)
      case ZoneIdData(_) => parseTime(alg, classOf[ZoneId], ZoneId.of)
      case ZonedDateTimeData(_) => parseTime(alg, classOf[ZonedDateTime], ZonedDateTime.parse(_, zonedDateTimeFormatter))
      case ZoneOffsetData(_) => parseTime(alg, classOf[ZoneOffset], ZoneOffset.of)
    }
  }
}
