package com.bones.interpreter.custom

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, RequiredValue}
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil

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


  def parseTime[A, OUT](
    baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT],
    alg: JavaTimeValue[A],
    clazz: Class[A],
    f: String => A,
    validations: List[ValidationOp[A]]
  ): (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (jsonOpt: Option[OUT], path: List[String]) =>
      {
        jsonOpt match {
          case Some(json) =>
            baseValidator
              .extractString(Right(alg), clazz)(json, path)
              .flatMap(result => errorHandleTimeParsing(path, f, result))
              .flatMap(result => ValidationUtil.validate(validations)(result, path))
          case None =>
            Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
        }
      }
  }

  def parseYear[A, OUT](
    baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT],
    alg: JavaTimeValue[A],
    validations: List[ValidationOp[Year]])
    : (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], Year] =
    (jsonOpt: Option[OUT], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseValidator
            .extractInt(Right(alg))(json, path)
            .map(result => Year.of(result))
            .flatMap(result => ValidationUtil.validate(validations)(result, path))
        case None =>
          Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
      }
    }
}

trait JavaTimeValidator[IN] extends InterchangeFormatValidator[JavaTimeValue, IN] {

  import JavaTimeValidator._

  val localDateFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter
  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val baseValidator: KvpInterchangeFormatValidatorInterpreter[IN]

  override def validate[A](alg: JavaTimeValue[A])
    : (Option[IN], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {

    alg match {
      case d: DateTimeExceptionData =>
        parseTime(
          baseValidator,
          alg,
          classOf[DateTimeException],
          input => new DateTimeException(input),
          d.validations)
      case d: DayOfWeekData =>
        parseTime(baseValidator, alg, classOf[DayOfWeek], DayOfWeek.valueOf, d.validations)
      case d: DurationData =>
        parseTime(baseValidator, alg, classOf[Duration], Duration.parse, d.validations)
      case i: InstantData =>
        parseTime(
          baseValidator,
          alg,
          classOf[Instant],
          input => Instant.from(instantFormatter.parse(input)),
          i.validations)
      case op @ LocalDateTimeData(validations) =>
        parseTime(
          baseValidator,
          alg,
          classOf[LocalDateTime],
          LocalDateTime.parse(_, localDateTimeFormatter),
          op.validations
        )
      case op @ LocalDateData(validations) =>
        parseTime(
          baseValidator,
          alg,
          classOf[LocalDate],
          LocalDate.parse(_, localDateFormatter),
          op.validations
        )
      case op @ LocalTimeData(validations) =>
        parseTime(
          baseValidator,
          alg,
          classOf[LocalTime],
          LocalTime.parse(_, localTimeFormatter),
          op.validations
        )
      case m: MonthData =>
        parseTime(baseValidator, alg, classOf[Month], Month.valueOf, m.validations)
      case m: MonthDayData =>
        parseTime(baseValidator, alg, classOf[MonthDay], MonthDay.parse, m.validations)
      case o: OffsetDateTimeData =>
        parseTime(
          baseValidator,
          alg,
          classOf[OffsetDateTime],
          OffsetDateTime.parse(_, offsetDateTimeFormatter),
          o.validations)
      case o: OffsetTimeData =>
        parseTime(
          baseValidator,
          alg,
          classOf[OffsetTime],
          OffsetTime.parse(_, offsetTimeFormatter),
          o.validations)
      case p: PeriodData =>
        parseTime(baseValidator, alg, classOf[Period], Period.parse, p.validations)
      case y: YearData => parseYear(baseValidator, alg, y.validations)
      case y: YearMonthData =>
        parseTime(baseValidator, alg, classOf[YearMonth], YearMonth.parse, y.validations)
      case z: ZoneIdData =>
        parseTime(baseValidator, alg, classOf[ZoneId], ZoneId.of, z.validations)
      case z: ZonedDateTimeData =>
        parseTime(
          baseValidator,
          alg,
          classOf[ZonedDateTime],
          ZonedDateTime.parse(_, zonedDateTimeFormatter),
          z.validations)
      case z: ZoneOffsetData =>
        parseTime(baseValidator, alg, classOf[ZoneOffset], ZoneOffset.of, z.validations)
    }
  }

}
