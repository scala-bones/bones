package com.bones.data.custom

import java.time._

import com.bones.data.{KvpValue}
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.custom.JavaTimeValidation
import com.bones.validation.custom.JavaTimeValidation._
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

sealed abstract class JavaTimeValue[A: Manifest] extends KvpValue[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

/** Note that Deserializing DateTimeException, although support throughout, will contain a misleading stack trace.  This is because
  * it is impossible to create a new DateTimeException without suppressing the stack trace.  This class is type
  * is mainly here for Serialization. */
final case class DateTimeExceptionData(validations: List[ValidationOp[DateTimeException]])
    extends JavaTimeValue[DateTimeException]

final case class DayOfWeekData(validations: List[ValidationOp[DayOfWeek]])
    extends JavaTimeValue[DayOfWeek]

final case class DurationData(validations: List[ValidationOp[Duration]])
    extends JavaTimeValue[Duration]

final case class InstantData(validations: List[ValidationOp[Instant]])
    extends JavaTimeValue[Instant]

final case class LocalDateTimeData(validations: List[ValidationOp[LocalDateTime]])
  extends JavaTimeValue[LocalDateTime]

final case class LocalDateData(validations: List[ValidationOp[LocalDate]])
  extends JavaTimeValue[LocalDate]

final case class LocalTimeData(validations: List[ValidationOp[LocalTime]])
  extends JavaTimeValue[LocalTime]

final case class MonthData(validations: List[ValidationOp[Month]])
    extends JavaTimeValue[Month]

final case class MonthDayData(validations: List[ValidationOp[MonthDay]])
    extends JavaTimeValue[MonthDay]

final case class OffsetDateTimeData(validations: List[ValidationOp[OffsetDateTime]])
    extends JavaTimeValue[OffsetDateTime]

final case class OffsetTimeData(validations: List[ValidationOp[OffsetTime]])
    extends JavaTimeValue[OffsetTime]

final case class PeriodData(validations: List[ValidationOp[Period]])
    extends JavaTimeValue[Period]

final case class YearData(validations: List[ValidationOp[Year]])
    extends JavaTimeValue[Year]

final case class YearMonthData(validations: List[ValidationOp[YearMonth]])
    extends JavaTimeValue[YearMonth]

final case class ZonedDateTimeData(validations: List[ValidationOp[ZonedDateTime]])
    extends JavaTimeValue[ZonedDateTime]

final case class ZoneIdData(validations: List[ValidationOp[ZoneId]])
    extends JavaTimeValue[ZoneId]

final case class ZoneOffsetData(validations: List[ValidationOp[ZoneOffset]])
    extends JavaTimeValue[ZoneOffset]

trait JavaTimeValidationSugar {

  val jt_dow: JavaTimeValidation.DayOfWeekValidation.type = DayOfWeekValidation
  val jt_d: JavaTimeValidation.DurationValidation.type = DurationValidation
  val jt_i: JavaTimeValidation.InstantValidation.type = InstantValidation
  val jt_m: JavaTimeValidation.MonthValidations.type = MonthValidations
  val jt_md: JavaTimeValidation.MonthDayValidations.type = MonthDayValidations
  val jt_ldt: JavaTimeValidation.LocalDateTimeValidation.type = LocalDateTimeValidation
  val jt_ld: JavaTimeValidation.LocalDateValidation.type = LocalDateValidation
  val jt_lt: JavaTimeValidation.LocalTimeValidation.type = LocalTimeValidation
  val jt_odt: JavaTimeValidation.OffsetDateTimeValidations.type = OffsetDateTimeValidations
  val jt_ot: JavaTimeValidation.OffsetTimeValidations.type = OffsetTimeValidations
  val jt_p: JavaTimeValidation.PeriodValidations.type = PeriodValidations
  val jt_y: JavaTimeValidation.YearValidations.type = YearValidations
  val jt_ym: JavaTimeValidation.YearMonthValidations.type = YearMonthValidations
  val jt_zdt: JavaTimeValidation.ZonedDateTimeValidations.type = ZonedDateTimeValidations
  val jt_zi: JavaTimeValidation.ZoneIdValidations.type = ZoneIdValidations
  val jt_zo: JavaTimeValidation.ZoneOffsetValidations.type = ZoneOffsetValidations

}

/** convenient methods for creating GADTs when this is the only custom algebra being used.  If using multiple custom
  * algebras, use [[JavaTimeValueSugarInjected]] to inject into a Coproduct context. */
trait JavaTimeValueSugar extends JavaTimeValidationSugar {
  def dateTimeException(validations: ValidationOp[DateTimeException]*): DateTimeExceptionData =
    DateTimeExceptionData(validations.toList)

  val dateTimeException: DateTimeExceptionData = dateTimeException()

  def dayOfWeek(validations: ValidationOp[DayOfWeek]*): DayOfWeekData =
    DayOfWeekData(validations.toList)
  val dayOfWeek: DayOfWeekData = dayOfWeek()

  def duration(validations: ValidationOp[Duration]*): DurationData =
    DurationData(validations.toList)
  val duration: DurationData = duration()

  def instant(validations: ValidationOp[Instant]*): InstantData = InstantData(validations.toList)
  val instant: InstantData = instant()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def localDateTime(v: ValidationOp[LocalDateTime]*): LocalDateTimeData =
    LocalDateTimeData(v.toList)

  val localDateTime: LocalDateTimeData = LocalDateTimeData(List.empty)

  def localDate(v: ValidationOp[LocalDate]*): LocalDateData = LocalDateData(v.toList)

  val localDate: LocalDateData = LocalDateData(List.empty)

  def localTime(v: ValidationOp[LocalTime]*): LocalTimeData = LocalTimeData(v.toList)

  val localTime: LocalTimeData = localTime()

  def month(validations: ValidationOp[Month]*): MonthData = MonthData(validations.toList)
  val month: MonthData = month()

  def monthDay(validations: ValidationOp[MonthDay]*): MonthDayData =
    MonthDayData(validations.toList)
  val monthDay: MonthDayData = monthDay()

  def offsetDateTime(validations: ValidationOp[OffsetDateTime]*): OffsetDateTimeData =
    OffsetDateTimeData(validations.toList)
  val offsetDateTime: OffsetDateTimeData = offsetDateTime()

  def offsetTime(validations: ValidationOp[OffsetTime]*): OffsetTimeData =
    OffsetTimeData(validations.toList)
  val offsetTime: OffsetTimeData = offsetTime()

  def period(validations: ValidationOp[Period]*): PeriodData = PeriodData(validations.toList)
  val period: PeriodData = period()

  def year(validations: ValidationOp[Year]*): YearData = YearData(validations.toList)
  val year: YearData = year()

  def yearMonth(validations: ValidationOp[YearMonth]*): YearMonthData =
    YearMonthData(validations.toList)
  val yearMonth: YearMonthData = yearMonth()

  def zonedDateTime(validations: ValidationOp[ZonedDateTime]*): ZonedDateTimeData =
    ZonedDateTimeData(validations.toList)
  val zonedDateTime: ZonedDateTimeData = zonedDateTime()

  def zoneId(validations: ValidationOp[ZoneId]*): ZoneIdData = ZoneIdData(validations.toList)
  val zoneId: ZoneIdData = zoneId()

  def zoneOffset(validations: ValidationOp[ZoneOffset]*): ZoneOffsetData =
    ZoneOffsetData(validations.toList)
  val zoneOffset: ZoneOffsetData = zoneOffset()
}

/** Adds smart constructors to lift our GADT into a multi-algebra coproduct */
trait JavaTimeValueSugarInjected[ALG[_] <: Coproduct] extends JavaTimeValidationSugar {

  def javaTimeInject[A]: Inject[ALG[A], JavaTimeValue[A]]

  def dateTimeException(validations: ValidationOp[DateTimeException]*): ALG[DateTimeException] =
    javaTimeInject(DateTimeExceptionData(validations.toList))

  val dateTimeException: ALG[DateTimeException] = dateTimeException()

  def dayOfWeek(validations: ValidationOp[DayOfWeek]*): ALG[DayOfWeek] =
    javaTimeInject[DayOfWeek].apply(DayOfWeekData(validations.toList))

  val dayOfWeek: ALG[DayOfWeek] = dayOfWeek()

  def duration(validations: ValidationOp[Duration]*): ALG[Duration] =
    javaTimeInject(DurationData(validations.toList))
  val duration: ALG[Duration] = duration()

  def instant(validations: ValidationOp[Instant]*): ALG[Instant] =
    javaTimeInject(InstantData(validations.toList))
  val instant: ALG[Instant] = instant()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def localDateTime(v: ValidationOp[LocalDateTime]*): ALG[LocalDateTime] =
    javaTimeInject(LocalDateTimeData(v.toList))

  val localDateTime: ALG[LocalDateTime] = localDateTime()

  def localDate(v: ValidationOp[LocalDate]*): ALG[LocalDate] =
    javaTimeInject(LocalDateData(v.toList))

  val localDate: ALG[LocalDate] = localDate()

  def localTime(v: ValidationOp[LocalTime]*): ALG[LocalTime] =
    javaTimeInject(LocalTimeData(v.toList))

  val localTime: ALG[LocalTime] = localTime()

  def month(validations: ValidationOp[Month]*): ALG[Month] =
    javaTimeInject(MonthData(validations.toList))
  val month: ALG[Month] = month()

  def monthDay(validations: ValidationOp[MonthDay]*): ALG[MonthDay] =
    javaTimeInject(MonthDayData(validations.toList))
  val monthDay: ALG[MonthDay] = monthDay()

  def offsetDateTime(validations: ValidationOp[OffsetDateTime]*): ALG[OffsetDateTime] =
    javaTimeInject(OffsetDateTimeData(validations.toList))
  val offsetDateTime: ALG[OffsetDateTime] = offsetDateTime()

  def offsetTime(validations: ValidationOp[OffsetTime]*): ALG[OffsetTime] =
    javaTimeInject(OffsetTimeData(validations.toList))
  val offsetTime: ALG[OffsetTime] = offsetTime()

  def period(validations: ValidationOp[Period]*): ALG[Period] =
    javaTimeInject(PeriodData(validations.toList))
  val period: ALG[Period] = period()

  def year(validations: ValidationOp[Year]*): ALG[Year] =
    javaTimeInject(YearData(validations.toList))
  val year: ALG[Year] = year()

  def yearMonth(validations: ValidationOp[YearMonth]*): ALG[YearMonth] =
    javaTimeInject(YearMonthData(validations.toList))
  val yearMonth: ALG[YearMonth] = yearMonth()

  def zonedDateTime(validations: ValidationOp[ZonedDateTime]*): ALG[ZonedDateTime] =
    javaTimeInject(ZonedDateTimeData(validations.toList))
  val zonedDateTime: ALG[ZonedDateTime] = zonedDateTime()

  def zoneId(validations: ValidationOp[ZoneId]*): ALG[ZoneId] =
    javaTimeInject(ZoneIdData(validations.toList))
  val zoneId: ALG[ZoneId] = zoneId()

  def zoneOffset(validations: ValidationOp[ZoneOffset]*): ALG[ZoneOffset] =
    javaTimeInject(ZoneOffsetData(validations.toList))

  val zoneOffset: ALG[ZoneOffset] = zoneOffset()
}
