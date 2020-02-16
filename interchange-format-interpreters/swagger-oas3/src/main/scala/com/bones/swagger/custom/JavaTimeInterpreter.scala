package com.bones.swagger.custom

import java.time._
import java.time.format.DateTimeFormatter

import com.bones.data.custom._
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name}
import io.swagger.v3.oas.models.media.Schema
import scala.collection.JavaConverters._

trait JavaTimeInterpreter extends CustomSwaggerInterpreter[JavaTimeValue] {

  import SwaggerCoreInterpreter._

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  val instantExample = Instant.ofEpochSecond(1581349194)
  val periodExample = Period.ofMonths(3)

  override def toSchema[A](alg: JavaTimeValue[A]): Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] =
    alg match {
      case dte: DateTimeExceptionData =>
        name => addStringSchema(name, "Error With Date Time", "The error message for a DateTimeException")
      case DayOfWeekData(_) =>
        name => addEnumerationData(name, "String representing the day of the week", DayOfWeek.FRIDAY.toString, DayOfWeek.values.toList.map(_.toString))
      case DurationData(_) =>
        name => addStringSchema(name, Duration.ofHours(24).toString, "string value of type duration")
      case InstantData(_) =>
        name => addStringSchema(name, instantFormatter.format(instantExample), "string value of an instant")
      case MonthData(_) =>
        name => addEnumerationData(name, Month.JANUARY.toString, "String representation of the month of year", Month.values.toList.map(_.toString))
      case MonthDayData(_) =>
        name => addStringSchema(name, MonthDay.of(Month.JANUARY, 1).toString, "string representation of month/day")
      case OffsetDateTimeData(_) =>
        name => addStringSchema(name, offsetDateTimeFormatter.format(instantExample), "string value of an offset date/time")
      case OffsetTimeData(_) =>
        name => addStringSchema(name, offsetTimeFormatter.format(instantExample), "string value of an offset time")
      case PeriodData(_) =>
        name => addStringSchema(name, periodExample.toString, "string value of a period")
      case YearData(_) =>
        name => addIntSchema(name, 2020, "int value representing the year")
      case YearMonthData(_) =>
        name => addStringSchema(name, YearMonth.of(2020, Month.JANUARY).toString, "string value representing the year/month")
      case ZoneIdData(_) =>
        name => addEnumerationData(name, ZoneId.systemDefault.toString, "string value representing the zone id offset", ZoneId.getAvailableZoneIds.asScala.toList)
      case ZonedDateTimeData(_) =>
        name => addStringSchema(name, zonedDateTimeFormatter.format(instantExample), "string value representing a zoned date time")
      case ZoneOffsetData(_) =>
        name => addStringSchema(name, ZoneOffset.UTC.toString, "string value representing a zone offst")
    }
}
