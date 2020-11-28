package com.bones.tapir.values

import java.time.format.DateTimeFormatter
import java.time._

import com.bones.data.values._
import com.bones.tapir.{DescriptionString, ExampleString, TapirValueTransformation}
import sttp.tapir.SchemaType
import sttp.tapir.SchemaType.{SInteger, SString}

trait JavaTimeTapirSchema extends TapirValueTransformation[JavaTimeValue] {

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localDateFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter

  override def toSchemaType[A](
    alg: JavaTimeValue[A],
    description: Option[String],
    example: Option[A]): (SchemaType, DescriptionString, ExampleString) = {

    val tapirDescription =
      description.getOrElse(JavaTimeValueDefaultJsonMetadata.getDefaultDescription(alg))
    val tapirExample = example.getOrElse(
      JavaTimeValueDefaultJsonMetadata.getDefaultExample(alg)
    )

    val (schemaType, tapirExampleString) = alg match {
      case dt: DateTimeExceptionData =>
        (SString, tapirExample.asInstanceOf[DateTimeException].getMessage)
      case dt: DayOfWeekData =>
        (SInteger, tapirExample.toString)
      case dd: DurationData =>
        (SString, tapirExample.toString)
      case id: InstantData =>
        (SString, instantFormatter.format(tapirExample.asInstanceOf[Instant]))
      case dd: LocalDateTimeData =>
        (SString, localDateTimeFormatter.format(tapirExample.asInstanceOf[LocalDateTime]))
      case dt: LocalDateData =>
        (SString, localDateFormatter.format(tapirExample.asInstanceOf[LocalDate]))
      case lt: LocalTimeData =>
        (SString, localTimeFormatter.format(tapirExample.asInstanceOf[LocalTime]))
      case md: MonthData =>
        (SInteger, tapirExample.toString)
      case md: MonthDayData =>
        (SInteger, tapirExample.toString)
      case dt: OffsetDateTimeData =>
        (SString, offsetDateTimeFormatter.format(tapirExample.asInstanceOf[OffsetDateTime]))
      case dt: OffsetTimeData =>
        (SString, offsetTimeFormatter.format(tapirExample.asInstanceOf[OffsetTime]))
      case pd: PeriodData =>
        (SString, tapirExample.toString)
      case yd: YearData =>
        (SInteger, tapirExample.asInstanceOf[Year].getValue.toString)
      case ym: YearMonthData =>
        (SString, tapirExample.toString)
      case zd: ZonedDateTimeData =>
        (SString, zonedDateTimeFormatter.format(tapirExample.asInstanceOf[ZonedDateTime]))
      case zi: ZoneIdData =>
        (SString, tapirExample.toString)
      case zo: ZoneOffsetData =>
        (SString, tapirExample.toString)
    }

    (schemaType, tapirDescription, tapirExampleString)
  }
}
