package com.bones.data.custom

import java.time._

import com.bones.data.{AlgToCollectionData, HasManifest, ListData}
import com.bones.validation.ValidationDefinition.ValidationOp

sealed abstract class JavaTimeValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

final case class DateTimeExceptionData(validationOp: ValidationOp[DateTimeException])
  extends JavaTimeValue[DateTimeException]
    with AlgToCollectionData[JavaTimeValue, DateTimeException, DateTimeExceptionData]

final case class DayOfWeekData(validationOp: ValidationOp[DayOfWeek])
  extends JavaTimeValue[DayOfWeek]
    with AlgToCollectionData[JavaTimeValue, DayOfWeek, DayOfWeekData]

final case class DurationData(validationOp: ValidationOp[Duration])
  extends JavaTimeValue[Duration]
    with AlgToCollectionData[JavaTimeValue, Duration, DurationData]

final case class InstantData(validationOp: ValidationOp[Instant])
  extends JavaTimeValue[Instant]
    with AlgToCollectionData[JavaTimeValue, Instant, InstantData]

final case class MonthData(validationOp: ValidationOp[Month])
  extends JavaTimeValue[Month]
    with AlgToCollectionData[JavaTimeValue, Month, MonthData]

final case class MonthDayData(validationOp: ValidationOp[MonthDay])
  extends JavaTimeValue[MonthDay]
    with AlgToCollectionData[JavaTimeValue, MonthDay, MonthDayData]

final case class OffsetDateTimeData(validationOp: ValidationOp[OffsetDateTime])
  extends JavaTimeValue[OffsetDateTime]
    with AlgToCollectionData[JavaTimeValue, OffsetDateTime, OffsetDateTimeData]

final case class OffsetTimeData(validationOp: ValidationOp[OffsetTime])
  extends JavaTimeValue[OffsetTime]
    with AlgToCollectionData[JavaTimeValue, OffsetTime, OffsetTimeData]

final case class PeriodData(validationOp: ValidationOp[Period])
  extends JavaTimeValue[Period]
    with AlgToCollectionData[JavaTimeValue, Period, PeriodData]

final case class YearData(validationOp: ValidationOp[Year])
  extends JavaTimeValue[Year]
    with AlgToCollectionData[JavaTimeValue, Year, YearData]

final case class YearMonthData(validationOp: ValidationOp[YearMonth])
  extends JavaTimeValue[YearMonth]
    with AlgToCollectionData[JavaTimeValue, YearMonth, YearMonthData]

final case class ZonedDateTimeData(validationOp: ValidationOp[ZonedDateTime])
  extends JavaTimeValue[ZonedDateTime]
    with AlgToCollectionData[JavaTimeValue, ZonedDateTime, ZonedDateTimeData]

final case class ZoneIdData(validationOp: ValidationOp[ZoneId])
  extends JavaTimeValue[ZoneId]
    with AlgToCollectionData[JavaTimeValue, ZoneId, ZoneIdData]

final case class ZoneOffsetData(validationOp: ValidationOp[ZoneOffset])
  extends JavaTimeValue[ZoneOffset]
    with AlgToCollectionData[JavaTimeValue, ZoneOffset, ZoneOffsetData]


