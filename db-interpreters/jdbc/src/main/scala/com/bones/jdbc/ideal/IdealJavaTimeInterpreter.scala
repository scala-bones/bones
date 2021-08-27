package com.bones.jdbc.ideal

import com.bones.data.values._
import com.bones.jdbc.findUniqueConstraint
import com.bones.si.ideal._

object IdealJavaTimeInterpreter
    extends IdealValue[JavaTimeValue]
    with BaseJavaTimeInterpreter[IdealDataType] {
  override def columns[A](
    alg: JavaTimeValue[A]
  ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
    TableCollection,
    List[UniqueGroup]
  ) = {
    val uniqueConstraint = findUniqueConstraint(alg.validations)
    defaultColumns(matchJavaTimeValue(alg), uniqueConstraint)
  }

  override def dateTimeExceptionData(dateTimeExceptionData: DateTimeExceptionData): IdealDataType =
    StringType.unbounded

  override def dayOfWeekData(dayOfWeekData: DayOfWeekData): IdealDataType = SmallIntType

  override def durationData(durationData: DurationData): IdealDataType = LongType()

  override def instantData(instantData: InstantData): IdealDataType = LongType()

  override def localDateTimeData(localDateTimeData: LocalDateTimeData): IdealDataType =
    TimestampType.withoutTimeZone()

  override def localDateData(localDateData: LocalDateData): IdealDataType =
    DateType

  override def localTimeData(localTimeData: LocalTimeData): IdealDataType =
    TimeType.withoutTimeZone()

  override def monthData(monthData: MonthData): IdealDataType = SmallIntType

  override def monthDayData(monthDayData: MonthDayData): IdealDataType = SmallIntType

  override def offsetDateTimeData(offsetDateTimeData: OffsetDateTimeData): IdealDataType =
    TimestampType.withTimeZone()

  override def offsetTimeData(offsetTimeData: OffsetTimeData): IdealDataType =
    TimeType.withTimeZone()

  override def periodData(periodData: PeriodData): IdealDataType =
    StringType.unbounded

  override def yearData(yearData: YearData): IdealDataType = IntegerType()

  override def yearMonthData(yearMonthData: YearMonthData): IdealDataType = StringType.unbounded

  override def zonedDateTimeData(zonedDateTimeData: ZonedDateTimeData): IdealDataType =
    TimestampType.withTimeZone()

  override def zoneIdData(zoneIdData: ZoneIdData): IdealDataType = StringType(10)

  override def zoneOffsetData(zoneOffsetData: ZoneOffsetData): IdealDataType = StringType.unbounded
}
