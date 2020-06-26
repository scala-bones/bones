package com.bones.interpreter.custom

import java.time.format.DateTimeFormatter

import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder

trait JavaTimeEncoder[OUT] extends InterchangeFormatEncoder[JavaTimeValue, OUT] {

  val baseEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]
  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localDateFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter


  override def encode[A](alg: JavaTimeValue[A]): A => OUT =
    alg match {
      case dte: DateTimeExceptionData =>
        val f = baseEncoder.stringToOut
        ex =>
          f(ex.getMessage)
      case DayOfWeekData(_) =>
        val f = baseEncoder.stringToOut
        dow =>
          f(dow.toString)
      case DurationData(_) =>
        val f = baseEncoder.stringToOut
        duration =>
          f(duration.toString)
      case InstantData(_) =>
        val f = baseEncoder.stringToOut
        instant =>
          f(instantFormatter.format(instant))
      case LocalDateTimeData(_) =>
        input =>
          baseEncoder.stringToOut(localDateTimeFormatter.format(input))
      case LocalDateData(_)     =>
        input =>
          baseEncoder.stringToOut(localDateFormatter.format(input))
      case LocalTimeData(_)     =>
        input =>
          baseEncoder.stringToOut(localTimeFormatter.format(input))
      case MonthData(_) =>
        val f = baseEncoder.stringToOut
        month =>
          f(month.toString)
      case MonthDayData(_) =>
        val f = baseEncoder.stringToOut
        monthDay =>
          f(monthDay.toString)
      case OffsetDateTimeData(_) =>
        val f = baseEncoder.stringToOut
        offset =>
          f(offsetDateTimeFormatter.format(offset))
      case OffsetTimeData(_) =>
        val f = baseEncoder.stringToOut
        offset =>
          f(offsetTimeFormatter.format(offset))
      case PeriodData(_) =>
        val f = baseEncoder.stringToOut
        period =>
          f(period.toString)
      case YearData(_) =>
        val f = baseEncoder.intToOut
        year =>
          f(year.getValue())
      case YearMonthData(_) =>
        val f = baseEncoder.stringToOut
        yearMonth =>
          f(yearMonth.toString)
      case ZoneIdData(_) =>
        val f = baseEncoder.stringToOut
        zoneId =>
          f(zoneId.toString)
      case ZonedDateTimeData(_) =>
        val f = baseEncoder.stringToOut
        dateTime =>
          f(zonedDateTimeFormatter.format(dateTime))
      case ZoneOffsetData(_) =>
        val f = baseEncoder.stringToOut
        zoneOffset =>
          f(zoneOffset.toString)
    }

}
