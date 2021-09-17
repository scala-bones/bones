package com.bones.interpreter.deltavalidator

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time._

import com.bones.data.Error.CanNotConvert
import com.bones.data.values._
import com.bones.validation.ValidationUtil

object JavaTimeValueInterpreter {
  def errorHandleTimeParsing[A, IN](f: () => A): DeltaValueValidator[String, JavaTimeValue, A, IN] =
    (in: IN, key: String, path: List[String]) => {
      try {
        Right(Right(f()))
      } catch {
        case ex: DateTimeParseException =>
          Left(List(CanNotConvert(path :+ key, in, classOf[LocalDateTime], Some(ex))))
        case ex: IllegalArgumentException =>
          Left(List(CanNotConvert(path :+ key, in, classOf[LocalDateTime], Some(ex))))
      }
    }

}

trait JavaTimeValueInterpreter[IN] extends InterchangeFormatDeltaValidatorValue[JavaTimeValue, IN] {
  import JavaTimeValueInterpreter._
  val primitive: PrimitiveInterchangeFormat[IN, String]
  val localDateFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter
  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  override def createDeltaValidator[A](
    alg: JavaTimeValue[A]
  ): DeltaValueValidator[String, JavaTimeValue, A, IN] = {

    def stringExtractor =
      primitive
        .extractString[JavaTimeValue](alg.typeName)

    val validator = alg match {
      case _: DateTimeExceptionData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => new DateTimeException(str)))
      case _: DayOfWeekData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => DayOfWeek.valueOf(str)))
      case _: DurationData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => Duration.parse(str)))
      case _: InstantData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => Instant.from(instantFormatter.parse(str))))
      case _: LocalDateTimeData =>
        stringExtractor
          .flatMapA(str =>
            errorHandleTimeParsing(() => LocalDateTime.parse(str, localDateTimeFormatter))
          )
      case _: LocalDateData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => LocalDate.parse(str, localDateFormatter)))
      case _: LocalTimeData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => LocalTime.parse(str, localTimeFormatter)))
      case _: MonthData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => Month.valueOf(str)))
      case _: MonthDayData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => MonthDay.parse(str)))
      case _: OffsetDateTimeData =>
        stringExtractor
          .flatMapA(str =>
            errorHandleTimeParsing(() => OffsetDateTime.parse(str, offsetDateTimeFormatter))
          )
      case _: OffsetTimeData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => OffsetTime.parse(str, offsetTimeFormatter)))
      case _: PeriodData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => Period.parse(str)))
      case _: YearData =>
        primitive.extractInt
          .flatMapA(i => errorHandleTimeParsing(() => Year.of(i)))
      case _: YearMonthData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => YearMonth.parse(str)))
      case z: ZoneIdData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => ZoneId.of(str)))
      case z: ZonedDateTimeData =>
        stringExtractor
          .flatMapA(str =>
            errorHandleTimeParsing(() => ZonedDateTime.parse(str, zonedDateTimeFormatter))
          )
      case z: ZoneOffsetData =>
        stringExtractor
          .flatMapA(str => errorHandleTimeParsing(() => ZoneOffset.of(str)))
    }

    validator
      .asInstanceOf[DeltaValueValidator[String, JavaTimeValue, A, IN]]
      .addValidation(ValidationUtil.validate(alg.validations))

  }
}
