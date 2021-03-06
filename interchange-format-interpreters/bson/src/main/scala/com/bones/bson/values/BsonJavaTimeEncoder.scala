package com.bones.bson.values

import java.time.format.DateTimeFormatter
import java.time._

import com.bones.bson.{BsonEncoderInterpreter, BsonPrimitiveEncoder}
import com.bones.data.values._
import com.bones.interpreter.encoder.{Encoder, InterchangeFormatEncoderValue}
import reactivemongo.bson.{BSONDateTime, BSONLong, BSONValue}

trait BsonJavaTimeEncoder extends InterchangeFormatEncoderValue[JavaTimeValue, BSONValue] {

  val baseEncoder = BsonPrimitiveEncoder
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter

  override def generateEncoder[A](alg: JavaTimeValue[A]): Encoder[JavaTimeValue, A, BSONValue] =
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
        (input: Instant) =>
          BSONDateTime(input.toEpochMilli)
      case LocalDateTimeData(_) =>
        (input: LocalDateTime) =>
          val date = input.toInstant(ZoneOffset.UTC).toEpochMilli
          BSONDateTime(date)
      case LocalTimeData(_) =>
        (input: LocalTime) =>
          BSONLong(input.toNanoOfDay)
      case LocalDateData(_) =>
        (input: LocalDate) =>
          BSONDateTime(input.toEpochDay)
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
        (offset: OffsetDateTime) =>
          f(offsetDateTimeFormatter.format(offset))
      case OffsetTimeData(_) =>
        val f = baseEncoder.stringToOut
        (offset: OffsetTime) =>
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
