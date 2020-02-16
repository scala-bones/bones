package com.bones.interpreter.custom

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, RequiredValue}
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator

object JavaTimeValidator {

  def errorHandleTimeParsing[A](
                                 path: List[String],
                                 f: String => A,
                                 input: String): Either[NonEmptyList[Error.ExtractionError], A] =
    try {
      Right(f(input))
    } catch {
      case ex: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime], Some(ex))))
      case ex: IllegalArgumentException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime], Some(ex))))
    }

  def parseTime[A, OUT](baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT], alg: JavaTimeValue[A], clazz: Class[A], f: String => A)
  : (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (jsonOpt: Option[OUT], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseValidator
            .extractString(Right(alg), clazz)(json, path)
            .flatMap(result => errorHandleTimeParsing(path, f, result))
        case None =>
          Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
      }
    }
  }

  def parseYear[A, OUT](baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT], alg: JavaTimeValue[A])
  : (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], Year] =
    (jsonOpt: Option[OUT], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseValidator
            .extractInt(Right(alg))(json, path)
            .map(result => Year.of(result))
        case None =>
          Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
      }
    }
}

trait JavaTimeValidator[OUT] extends InterchangeFormatValidator[JavaTimeValue, OUT] {

  import JavaTimeValidator._

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT]


  override def validate[A](alg: JavaTimeValue[A])
  : (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {

    alg match {
      case DateTimeExceptionData(_) =>
        parseTime(baseValidator, alg, classOf[DateTimeException], input => new DateTimeException(input))
      case DayOfWeekData(_) => parseTime(baseValidator, alg, classOf[DayOfWeek], DayOfWeek.valueOf)
      case DurationData(_) => parseTime(baseValidator, alg, classOf[Duration], Duration.parse)
      case InstantData(_) =>
        parseTime(baseValidator, alg, classOf[Instant], input => Instant.from(instantFormatter.parse(input)))
      case MonthData(_) => parseTime(baseValidator, alg, classOf[Month], Month.valueOf)
      case MonthDayData(_) => parseTime(baseValidator, alg, classOf[MonthDay], MonthDay.parse)
      case OffsetDateTimeData(_) =>
        parseTime(baseValidator, alg, classOf[OffsetDateTime], OffsetDateTime.parse(_, offsetDateTimeFormatter))
      case OffsetTimeData(_) =>
        parseTime(baseValidator, alg, classOf[OffsetTime], OffsetTime.parse(_, offsetTimeFormatter))
      case PeriodData(_) => parseTime(baseValidator, alg, classOf[Period], Period.parse)
      case YearData(_) => parseYear(baseValidator, alg)
      case YearMonthData(_) => parseTime(baseValidator, alg, classOf[YearMonth], YearMonth.parse)
      case ZoneIdData(_) => parseTime(baseValidator, alg, classOf[ZoneId], ZoneId.of)
      case ZonedDateTimeData(_) =>
        parseTime(baseValidator, alg, classOf[ZonedDateTime], ZonedDateTime.parse(_, zonedDateTimeFormatter))
      case ZoneOffsetData(_) => parseTime(baseValidator, alg, classOf[ZoneOffset], ZoneOffset.of)
    }
  }

}
