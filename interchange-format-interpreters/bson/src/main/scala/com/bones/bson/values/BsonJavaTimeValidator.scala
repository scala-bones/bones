package com.bones.bson.values

import java.time._
import java.time.format.DateTimeFormatter

import com.bones.bson.BsonPrimitiveValidator
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values._
import com.bones.interpreter.{InterchangeFormatValidatorValue, OptionalInputValidator, Validator}
import com.bones.interpreter.values.JavaTimeValidator.{parseTime, parseYear}
import reactivemongo.bson.{BSONDateTime, BSONLong, BSONValue}

trait BsonJavaTimeValidator extends InterchangeFormatValidatorValue[JavaTimeValue, BSONValue] {

  val baseValidator = BsonPrimitiveValidator

  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  def extractLocalDateTime[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[ExtractionErrors[String], LocalDateTime] =
    in match {
      case BSONDateTime(date) =>
        val i = Instant.ofEpochMilli(date)
        Right(LocalDateTime.ofInstant(i, ZoneOffset.UTC))
      case x => baseValidator.invalidValue(x, "DateTime", path)
    }

  def extractLocalDate[ALG[_], A](op: ALG[A]): Validator[String, ALG, LocalDate, BSONValue] = {
    (in: BSONValue, path: List[String]) =>
      in match {
        case BSONDateTime(date) =>
          Right(LocalDate.ofEpochDay(date))
        case x => baseValidator.invalidValue(x, "DateTime", path)
      }
  }

  def extractLocalTime[ALG[_], A](
    op: ALG[A])(in: BSONValue, path: List[String]): Either[ExtractionErrors[String], LocalTime] =
    in match {
      case BSONLong(time) => Right(LocalTime.ofNanoOfDay(time))
      case x              => baseValidator.invalidValue(x, "Long", path)
    }

  override def createValidator[A](
    alg: JavaTimeValue[A]): OptionalInputValidator[String, JavaTimeValue, A, BSONValue] =
    alg match {
      case d: DateTimeExceptionData =>
        parseTime(baseValidator, alg, input => new DateTimeException(input), d.validations)
      case d: DayOfWeekData =>
        parseTime(baseValidator, alg, DayOfWeek.valueOf, d.validations)
      case d: DurationData =>
        parseTime(baseValidator, alg, Duration.parse, d.validations)
      case id: InstantData =>
        val f: Validator[String, JavaTimeValue, Instant, BSONValue] =
          (in: BSONValue, path: List[String]) =>
            in match {
              case BSONDateTime(date) =>
                Right(Instant.ofEpochMilli(date))
              case x => baseValidator.invalidValue(x, "BSONDateTime", path)
          }
        baseValidator.required(id.typeName, id.validations, f)
      case ld: LocalDateTimeData =>
        baseValidator
          .required[JavaTimeValue, LocalDateTime](
            ld.typeName,
            ld.validations,
            extractLocalDateTime(ld))
      case ld: LocalDateData =>
        baseValidator.required(ld.typeName, ld.validations, extractLocalDate(ld))
      case lt: LocalTimeData =>
        baseValidator
          .required[JavaTimeValue, LocalTime](lt.typeName, lt.validations, extractLocalTime(lt))
      case md: MonthData =>
        parseTime(baseValidator, alg, Month.valueOf, md.validations)
      case md: MonthDayData =>
        parseTime(baseValidator, alg, MonthDay.parse, md.validations)
      case od: OffsetDateTimeData =>
        parseTime(
          baseValidator,
          alg,
          OffsetDateTime.parse(_, offsetDateTimeFormatter),
          od.validations)
      case ot: OffsetTimeData =>
        parseTime(baseValidator, alg, OffsetTime.parse(_, offsetTimeFormatter), ot.validations)
      case pd: PeriodData =>
        parseTime(baseValidator, alg, Period.parse, pd.validations)
      case yd: YearData => parseYear(baseValidator, alg, yd.validations)
      case ym: YearMonthData =>
        parseTime(baseValidator, alg, YearMonth.parse, ym.validations)
      case zi: ZoneIdData =>
        parseTime(baseValidator, alg, ZoneId.of, zi.validations)
      case z: ZonedDateTimeData =>
        parseTime(baseValidator, alg, ZonedDateTime.parse(_, zonedDateTimeFormatter), z.validations)
      case z: ZoneOffsetData =>
        parseTime(baseValidator, alg, ZoneOffset.of, z.validations)
    }

}
