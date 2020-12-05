package com.bones.interpreter.values

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import com.bones.data.Error.{CanNotConvert, ExtractionErrors, RequiredValue}
import com.bones.data.values._
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  OptionalInputValidator
}
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil

object JavaTimeValidator {

  def errorHandleTimeParsing[A](
    path: List[String],
    f: String => A,
    input: String): Either[ExtractionErrors[String], A] =
    try {
      Right(f(input))
    } catch {
      case ex: DateTimeParseException =>
        Left(List(CanNotConvert(path, input, classOf[LocalDateTime], Some(ex))))
      case ex: IllegalArgumentException =>
        Left(List(CanNotConvert(path, input, classOf[LocalDateTime], Some(ex))))
    }

  def parseTime[ALG[_], A, IN](
    baseValidator: InterchangeFormatPrimitiveValidator[IN],
    javaTimeAlgebra: JavaTimeValue[A],
    f: String => A,
    validations: List[ValidationOp[A]]
  ): OptionalInputValidator[String, JavaTimeValue, A, IN] = {
    (jsonOpt: Option[IN], path: List[String]) =>
      {
        jsonOpt match {
          case Some(json) =>
            baseValidator
              .extractString(javaTimeAlgebra.typeName)
              .validateWithPath(json, path)
              .flatMap(result => errorHandleTimeParsing(path, f, result))
              .flatMap(result => ValidationUtil.validate(validations)(result, path))
          case None =>
            Left(List(RequiredValue(path, javaTimeAlgebra.typeName)))
        }
      }
  }

  def parseYear[ALG[_], A, IN](
    baseValidator: InterchangeFormatPrimitiveValidator[IN],
    javaTimeValue: JavaTimeValue[A],
    validations: List[ValidationOp[Year]])
    : OptionalInputValidator[String, JavaTimeValue, Year, IN] =
    (jsonOpt: Option[IN], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseValidator.extractInt
            .validateWithPath(json, path)
            .map(result => Year.of(result))
            .flatMap(result => ValidationUtil.validate(validations)(result, path))
        case None =>
          Left(List(RequiredValue(path, javaTimeValue.typeName)))
      }
    }
}

trait JavaTimeValidator[IN] extends InterchangeFormatValidatorValue[JavaTimeValue, IN] {

  import JavaTimeValidator._

  val localDateFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter
  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def createValidator[A](
    alg: JavaTimeValue[A]): OptionalInputValidator[String, JavaTimeValue, A, IN] = {

    alg match {
      case d: DateTimeExceptionData =>
        parseTime(baseValidator, alg, input => new DateTimeException(input), d.validations)
      case d: DayOfWeekData =>
        parseTime(baseValidator, alg, DayOfWeek.valueOf, d.validations)
      case d: DurationData =>
        parseTime(baseValidator, alg, Duration.parse, d.validations)
      case i: InstantData =>
        parseTime(
          baseValidator,
          alg,
          input => Instant.from(instantFormatter.parse(input)),
          i.validations)
      case op @ LocalDateTimeData(validations) =>
        parseTime(
          baseValidator,
          alg,
          LocalDateTime.parse(_, localDateTimeFormatter),
          op.validations
        )
      case op @ LocalDateData(validations) =>
        parseTime(
          baseValidator,
          alg,
          LocalDate.parse(_, localDateFormatter),
          op.validations
        )
      case op @ LocalTimeData(validations) =>
        parseTime(
          baseValidator,
          alg,
          LocalTime.parse(_, localTimeFormatter),
          op.validations
        )
      case m: MonthData =>
        parseTime(baseValidator, alg, Month.valueOf, m.validations)
      case m: MonthDayData =>
        parseTime(baseValidator, alg, MonthDay.parse, m.validations)
      case o: OffsetDateTimeData =>
        parseTime(
          baseValidator,
          alg,
          OffsetDateTime.parse(_, offsetDateTimeFormatter),
          o.validations)
      case o: OffsetTimeData =>
        parseTime(baseValidator, alg, OffsetTime.parse(_, offsetTimeFormatter), o.validations)
      case p: PeriodData =>
        parseTime(baseValidator, alg, Period.parse, p.validations)
      case y: YearData => parseYear(baseValidator, alg, y.validations)
      case y: YearMonthData =>
        parseTime(baseValidator, alg, YearMonth.parse, y.validations)
      case z: ZoneIdData =>
        parseTime(baseValidator, alg, ZoneId.of, z.validations)
      case z: ZonedDateTimeData =>
        parseTime(baseValidator, alg, ZonedDateTime.parse(_, zonedDateTimeFormatter), z.validations)
      case z: ZoneOffsetData =>
        parseTime(baseValidator, alg, ZoneOffset.of, z.validations)
    }
  }

}
