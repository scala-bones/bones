package com.bones.bson.custom

import java.time._
import java.time.format.DateTimeFormatter

import cats.data.NonEmptyList
import com.bones.bson.BsonValidatorInterpreter
import com.bones.data.Error
import com.bones.data.Error.ExtractionError
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.custom.JavaTimeValidator.{parseTime, parseYear}
import reactivemongo.bson.{BSONDateTime, BSONValue}

trait BsonJavaTimeValidator extends InterchangeFormatValidator[JavaTimeValue, BSONValue] {

  val baseValidator = BsonValidatorInterpreter

  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  override def validate[A](alg: JavaTimeValue[A]): (Option[BSONValue], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] =
    alg match {
      case DateTimeExceptionData(_) =>
        parseTime(baseValidator, alg, classOf[DateTimeException], input => new DateTimeException(input))
      case DayOfWeekData(_) => parseTime(baseValidator, alg, classOf[DayOfWeek], DayOfWeek.valueOf)
      case DurationData(_) => parseTime(baseValidator, alg, classOf[Duration], Duration.parse)
      case id: InstantData =>
        val f = (in: BSONValue,
                 path: List[String]) =>
          in match {
            case BSONDateTime(date) =>
              Right(Instant.ofEpochMilli(date))
            case x => baseValidator.invalidValue(x, classOf[BSONDateTime], path)
          }
        baseValidator.required(Right(id), id.validations, f)
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
