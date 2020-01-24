package com.bones.argonaut.custom

import java.time.Instant
import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder

/** Encoder which uses default ISO formatters for each formatter */
trait IsoTimeEncoder extends TimeEncoder {
  override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
  override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
  override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
  override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
}

/** Encodes JavaTimeValue algebra into Json values */
trait TimeEncoder extends InterchangeFormatEncoder[JavaTimeValue, Json] {

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  override def encode[A](alg: JavaTimeValue[A]): A => Json =
    alg match {
      case DateTimeExceptionData(_) => ex => Json.jString(ex.getMessage())
      case DayOfWeekData(_) => dow => Json.jString(dow.toString)
      case DurationData(_) => duration => Json.jString(duration.toString)
      case InstantData(_) => instant => Json.jString(instantFormatter.format(instant))
      case MonthData(_) => month => Json.jString(month.toString)
      case MonthDayData(_) => monthDay => Json.jString(monthDay.toString)
      case OffsetDateTimeData(_) => offset => Json.jString(offsetDateTimeFormatter.format(offset))
      case OffsetTimeData(_) => offset => Json.jString(offsetTimeFormatter.format(offset))
      case PeriodData(_) => period => Json.jString(period.toString)
      case YearData(_) => year => Json.jNumber(year.getValue())
      case YearMonthData(_) => yearMonth => Json.jString(yearMonth.toString)
      case ZoneIdData(_) => zoneId => Json.jString(zoneId.toString)
      case ZonedDateTimeData(_) => dateTime => Json.jString(zonedDateTimeFormatter.format(dateTime))
      case ZoneOffsetData(_) => zoneOffset => Json.jString(zoneOffset.toString)
    }
}
