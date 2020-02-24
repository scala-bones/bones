package com.bones.data.custom

import java.time._

import com.bones.data.{AlgToCollectionData, HasManifest}
import com.bones.validation.ValidationDefinition.ValidationOp

sealed abstract class JavaTimeValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

/** Note that Deserializing DateTimeException, although support throughout, will contain a misleading stack trace.  This is because
  * it is impossible to create a new DateTimeException without suppressing the stack trace.  This class is type
  * is mainly here for Serialization. */
final case class DateTimeExceptionData(validations: List[ValidationOp[DateTimeException]])
    extends JavaTimeValue[DateTimeException]
    with AlgToCollectionData[JavaTimeValue, DateTimeException, DateTimeExceptionData]

final case class DayOfWeekData(validations: List[ValidationOp[DayOfWeek]])
    extends JavaTimeValue[DayOfWeek]
    with AlgToCollectionData[JavaTimeValue, DayOfWeek, DayOfWeekData]

final case class DurationData(validations: List[ValidationOp[Duration]])
    extends JavaTimeValue[Duration]
    with AlgToCollectionData[JavaTimeValue, Duration, DurationData]

final case class InstantData(validations: List[ValidationOp[Instant]])
    extends JavaTimeValue[Instant]
    with AlgToCollectionData[JavaTimeValue, Instant, InstantData]

final case class MonthData(validations: List[ValidationOp[Month]])
    extends JavaTimeValue[Month]
    with AlgToCollectionData[JavaTimeValue, Month, MonthData]

final case class MonthDayData(validations: List[ValidationOp[MonthDay]])
    extends JavaTimeValue[MonthDay]
    with AlgToCollectionData[JavaTimeValue, MonthDay, MonthDayData]

final case class OffsetDateTimeData(validations: List[ValidationOp[OffsetDateTime]])
    extends JavaTimeValue[OffsetDateTime]
    with AlgToCollectionData[JavaTimeValue, OffsetDateTime, OffsetDateTimeData]

final case class OffsetTimeData(validations: List[ValidationOp[OffsetTime]])
    extends JavaTimeValue[OffsetTime]
    with AlgToCollectionData[JavaTimeValue, OffsetTime, OffsetTimeData]

final case class PeriodData(validations: List[ValidationOp[Period]])
    extends JavaTimeValue[Period]
    with AlgToCollectionData[JavaTimeValue, Period, PeriodData]

final case class YearData(validations: List[ValidationOp[Year]])
    extends JavaTimeValue[Year]
    with AlgToCollectionData[JavaTimeValue, Year, YearData]

final case class YearMonthData(validations: List[ValidationOp[YearMonth]])
    extends JavaTimeValue[YearMonth]
    with AlgToCollectionData[JavaTimeValue, YearMonth, YearMonthData]

final case class ZonedDateTimeData(validations: List[ValidationOp[ZonedDateTime]])
    extends JavaTimeValue[ZonedDateTime]
    with AlgToCollectionData[JavaTimeValue, ZonedDateTime, ZonedDateTimeData]

final case class ZoneIdData(validations: List[ValidationOp[ZoneId]])
    extends JavaTimeValue[ZoneId]
    with AlgToCollectionData[JavaTimeValue, ZoneId, ZoneIdData]

final case class ZoneOffsetData(validations: List[ValidationOp[ZoneOffset]])
    extends JavaTimeValue[ZoneOffset]
    with AlgToCollectionData[JavaTimeValue, ZoneOffset, ZoneOffsetData]

trait JavaTimeValueSugar {
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
