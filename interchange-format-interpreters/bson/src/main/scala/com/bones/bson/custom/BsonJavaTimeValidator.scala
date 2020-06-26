package com.bones.bson.custom

import java.time._
import java.time.format.DateTimeFormatter

import cats.data.NonEmptyList
import com.bones.bson.BsonValidatorInterpreter
import com.bones.data.Error
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.custom.JavaTimeValidator.{parseTime, parseYear}
import reactivemongo.bson.{BSONDateTime, BSONLong, BSONValue}

trait BsonJavaTimeValidator extends InterchangeFormatValidator[JavaTimeValue, BSONValue] {

  val baseValidator = BsonValidatorInterpreter

  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  def extractLocalDateTime[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[Error.ExtractionError], LocalDateTime] =
    in match {
      case BSONDateTime(date) =>
        val i = Instant.ofEpochMilli(date)
        Right(LocalDateTime.ofInstant(i, ZoneOffset.UTC))
      case x => BsonValidatorInterpreter.invalidValue(x, classOf[BSONDateTime], path)
    }

  def extractLocalDate[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[Error.ExtractionError], LocalDate] =
    in match {
      case BSONDateTime(date) =>
        Right(LocalDate.ofEpochDay(date))
      case x => BsonValidatorInterpreter.invalidValue(x, classOf[BSONDateTime], path)
    }

  def extractLocalTime[ALG[_], A](op: ALG[A])(
    in: BSONValue,
    path: List[String]): Either[NonEmptyList[Error.ExtractionError], LocalTime] =
    in match {
      case BSONLong(time) => Right(LocalTime.ofNanoOfDay(time))
      case x              => BsonValidatorInterpreter.invalidValue(x, classOf[BSONLong], path)
    }

  override def validate[A](alg: JavaTimeValue[A]): (Option[BSONValue], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] =
    alg match {
      case d: DateTimeExceptionData =>
        parseTime(baseValidator, alg, classOf[DateTimeException], input => new DateTimeException(input), d.validations)
      case d: DayOfWeekData => parseTime(baseValidator, alg, classOf[DayOfWeek], DayOfWeek.valueOf, d.validations)
      case d: DurationData => parseTime(baseValidator, alg, classOf[Duration], Duration.parse, d.validations)
      case id: InstantData =>
        val f = (in: BSONValue,
                 path: List[String]) =>
          in match {
            case BSONDateTime(date) =>
              Right(Instant.ofEpochMilli(date))
            case x => baseValidator.invalidValue(x, classOf[BSONDateTime], path)
          }
        baseValidator.required(Right(id), id.validations, f)
      case ld: LocalDateTimeData => baseValidator.required(Right(ld), ld.validations, extractLocalDateTime(ld))
      case ld: LocalDateData => baseValidator.required(Right(ld), ld.validations, extractLocalDate(ld))
      case lt: LocalTimeData => baseValidator.required(Right(lt), lt.validations, extractLocalTime(lt))
      case md: MonthData => parseTime(baseValidator, alg, classOf[Month], Month.valueOf, md.validations)
      case md: MonthDayData => parseTime(baseValidator, alg, classOf[MonthDay], MonthDay.parse, md.validations)
      case od: OffsetDateTimeData =>
        parseTime(baseValidator, alg, classOf[OffsetDateTime], OffsetDateTime.parse(_, offsetDateTimeFormatter), od.validations)
      case ot: OffsetTimeData =>
        parseTime(baseValidator, alg, classOf[OffsetTime], OffsetTime.parse(_, offsetTimeFormatter), ot.validations)
      case pd: PeriodData => parseTime(baseValidator, alg, classOf[Period], Period.parse, pd.validations)
      case yd: YearData => parseYear(baseValidator, alg, yd.validations)
      case ym: YearMonthData => parseTime(baseValidator, alg, classOf[YearMonth], YearMonth.parse, ym.validations)
      case zi: ZoneIdData => parseTime(baseValidator, alg, classOf[ZoneId], ZoneId.of, zi.validations)
      case z: ZonedDateTimeData =>
        parseTime(baseValidator, alg, classOf[ZonedDateTime], ZonedDateTime.parse(_, zonedDateTimeFormatter), z.validations)
      case z: ZoneOffsetData => parseTime(baseValidator, alg, classOf[ZoneOffset], ZoneOffset.of, z.validations)
    }

}
