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
