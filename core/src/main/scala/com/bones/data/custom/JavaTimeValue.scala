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
final case class DateTimeExceptionData(validationOp: List[ValidationOp[DateTimeException]])
    extends JavaTimeValue[DateTimeException]
    with AlgToCollectionData[JavaTimeValue, DateTimeException, DateTimeExceptionData]

final case class DayOfWeekData(validationOp: List[ValidationOp[DayOfWeek]])
    extends JavaTimeValue[DayOfWeek]
    with AlgToCollectionData[JavaTimeValue, DayOfWeek, DayOfWeekData]

final case class DurationData(validationOp: List[ValidationOp[Duration]])
    extends JavaTimeValue[Duration]
    with AlgToCollectionData[JavaTimeValue, Duration, DurationData]

final case class InstantData(validationOp: List[ValidationOp[Instant]])
    extends JavaTimeValue[Instant]
    with AlgToCollectionData[JavaTimeValue, Instant, InstantData]

final case class MonthData(validationOp: List[ValidationOp[Month]])
    extends JavaTimeValue[Month]
    with AlgToCollectionData[JavaTimeValue, Month, MonthData]

final case class MonthDayData(validationOp: List[ValidationOp[MonthDay]])
    extends JavaTimeValue[MonthDay]
    with AlgToCollectionData[JavaTimeValue, MonthDay, MonthDayData]

final case class OffsetDateTimeData(validationOp: List[ValidationOp[OffsetDateTime]])
    extends JavaTimeValue[OffsetDateTime]
    with AlgToCollectionData[JavaTimeValue, OffsetDateTime, OffsetDateTimeData]

final case class OffsetTimeData(validationOp: List[ValidationOp[OffsetTime]])
    extends JavaTimeValue[OffsetTime]
    with AlgToCollectionData[JavaTimeValue, OffsetTime, OffsetTimeData]

final case class PeriodData(validationOp: List[ValidationOp[Period]])
    extends JavaTimeValue[Period]
    with AlgToCollectionData[JavaTimeValue, Period, PeriodData]

final case class YearData(validationOp: List[ValidationOp[Year]])
    extends JavaTimeValue[Year]
    with AlgToCollectionData[JavaTimeValue, Year, YearData]

final case class YearMonthData(validationOp: List[ValidationOp[YearMonth]])
    extends JavaTimeValue[YearMonth]
    with AlgToCollectionData[JavaTimeValue, YearMonth, YearMonthData]

final case class ZonedDateTimeData(validationOp: List[ValidationOp[ZonedDateTime]])
    extends JavaTimeValue[ZonedDateTime]
    with AlgToCollectionData[JavaTimeValue, ZonedDateTime, ZonedDateTimeData]

final case class ZoneIdData(validationOp: List[ValidationOp[ZoneId]])
    extends JavaTimeValue[ZoneId]
    with AlgToCollectionData[JavaTimeValue, ZoneId, ZoneIdData]

final case class ZoneOffsetData(validationOp: List[ValidationOp[ZoneOffset]])
    extends JavaTimeValue[ZoneOffset]
    with AlgToCollectionData[JavaTimeValue, ZoneOffset, ZoneOffsetData]

trait JavaTimeValueSugar {
  def dateTimeException(validationOp: ValidationOp[DateTimeException]*): DateTimeExceptionData =
    DateTimeExceptionData(validationOp.toList)

  val dateTimeException: DateTimeExceptionData = dateTimeException()

  def dayOfWeek(validationOp: ValidationOp[DayOfWeek]*): DayOfWeekData =
    DayOfWeekData(validationOp.toList)
  val dayOfWeek: DayOfWeekData = dayOfWeek()

  def duration(validationOp: ValidationOp[Duration]*): DurationData =
    DurationData(validationOp.toList)
  val duration: DurationData = duration()

  def instant(validationOp: ValidationOp[Instant]*): InstantData = InstantData(validationOp.toList)
  val instant: InstantData = instant()

  def month(validationOp: ValidationOp[Month]*): MonthData = MonthData(validationOp.toList)
  val month: MonthData = month()

  def monthDay(validationOp: ValidationOp[MonthDay]*): MonthDayData =
    MonthDayData(validationOp.toList)
  val monthDay: MonthDayData = monthDay()

  def offsetDateTime(validationOp: ValidationOp[OffsetDateTime]*): OffsetDateTimeData =
    OffsetDateTimeData(validationOp.toList)
  val offsetDateTime: OffsetDateTimeData = offsetDateTime()

  def offsetTime(validationOp: ValidationOp[OffsetTime]*): OffsetTimeData =
    OffsetTimeData(validationOp.toList)
  val offsetTime: OffsetTimeData = offsetTime()

  def period(validationOp: ValidationOp[Period]*): PeriodData = PeriodData(validationOp.toList)
  val period: PeriodData = period()

  def year(validationOp: ValidationOp[Year]*): YearData = YearData(validationOp.toList)
  val year: YearData = year()

  def yearMonth(validationOp: ValidationOp[YearMonth]*): YearMonthData =
    YearMonthData(validationOp.toList)
  val yearMonth: YearMonthData = yearMonth()

  def zonedDateTime(validationOp: ValidationOp[ZonedDateTime]*): ZonedDateTimeData =
    ZonedDateTimeData(validationOp.toList)
  val zonedDateTime: ZonedDateTimeData = zonedDateTime()

  def zoneId(validationOp: ValidationOp[ZoneId]*): ZoneIdData = ZoneIdData(validationOp.toList)
  val zoneId: ZoneIdData = zoneId()

  def zoneOffset(validationOp: ValidationOp[ZoneOffset]*): ZoneOffsetData =
    ZoneOffsetData(validationOp.toList)
  val zoneOffset: ZoneOffsetData = zoneOffset()
}
