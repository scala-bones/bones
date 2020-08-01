package com.bones.swagger.values

import java.time._
import java.time.format.DateTimeFormatter

import com.bones.data.values._
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name, validations}
import com.bones.validation.ValidationDefinition.ValidationOp
import io.swagger.v3.oas.models.media.Schema

import scala.jdk.CollectionConverters._

trait JavaTimeInterpreter extends CustomSwaggerInterpreter[JavaTimeValue] {

  import SwaggerCoreInterpreter._

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localDateFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter

  private def localDateExample = LocalDate.of(1970, 1, 1)

  private def localTimeExample = LocalTime.of(12, 0, 0, 0)

  private def localDateTimeExample = LocalDateTime.of(localDateExample, localTimeExample)


  val instantExample = Instant.ofEpochSecond(1581349194)
  val offsetDateTimeExample = OffsetDateTime.ofInstant(instantExample, ZoneId.of("Z"))
  val periodExample = Period.ofMonths(3)
  val exception = new DateTimeException("The error message for a DateTimeException")

  // TODO add validation descriptions
  def validations[A](list: List[ValidationOp[A]]): Schema[_] => Schema[_] = identity

  override def toSchema[A](
    alg: JavaTimeValue[A],
    description: Option[String],
    example: Option[A]): Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] =
    alg match {
      case dte: DateTimeExceptionData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("Error With Date Time"),
            example.getOrElse(exception).toString,
            validations(dte.validations))
      case dow: DayOfWeekData =>
        name =>
          addEnumerationData(
            name,
            description.getOrElse("String representing the day of the week"),
            DayOfWeek.FRIDAY.toString,
            DayOfWeek.values.toList.map(_.toString),
            validations(dow.validations)
          )
      case d: DurationData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("string value of type duration"),
            example.getOrElse(Duration.ofHours(24)).toString,
            validations(d.validations)
          )
      case id: InstantData =>
        name =>
          addDateTimeSchema(
            name,
            description.getOrElse("string value of an instant"),
            instantFormatter.format(example.getOrElse(instantExample).asInstanceOf[Instant]),
            validations(id.validations)
          )
      case ldt: LocalDateTimeData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("value of type local date time"),
            localDateTimeFormatter.format(
              example.getOrElse(localDateTimeExample).asInstanceOf[LocalDateTime]),
            validations(ldt.validations)
          )
      case ld: LocalDateData =>
        name =>
          addDateSchema(
            name,
            description.getOrElse("value of type local date"),
            localDateFormatter.format(example.getOrElse(localDateExample).asInstanceOf[LocalDate]),
            validations(ld.validations)
          )
      case lt: LocalTimeData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("value of type local time"),
            localTimeFormatter.format(example.getOrElse(localTimeExample).asInstanceOf[LocalTime]),
            validations(lt.validations)
          )
      case md: MonthData =>
        name =>
          addEnumerationData(
            name,
            description.getOrElse("String representation of the month of year"),
            example.getOrElse(Month.JANUARY).toString,
            Month.values.toList.map(_.toString),
            validations(md.validations)
          )
      case md: MonthDayData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("string representation of month/day"),
            example.getOrElse(MonthDay.of(Month.JANUARY, 1)).toString,
            validations(md.validations)
          )
      case od: OffsetDateTimeData =>
        name =>
          addDateTimeSchema(
            name,
            description.getOrElse("string value of an offset date/time"),
            offsetDateTimeFormatter.format(
              example.getOrElse(offsetDateTimeExample).asInstanceOf[OffsetDateTime]),
            validations(od.validations)
          )
      case od: OffsetTimeData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("string value of an offset time"),
            offsetTimeFormatter.format(
              example.getOrElse(offsetDateTimeExample.toOffsetTime).asInstanceOf[OffsetTime]),
            validations(od.validations)
          )
      case pd: PeriodData =>
        name =>
          addStringSchema(
            name,
            "string value of a period",
            example.getOrElse(periodExample).toString,
            validations(pd.validations)
          )
      case yd: YearData =>
        name =>
          addIntSchema(
            name,
            description.getOrElse("int value representing the year"),
            example.getOrElse(Year.of(2020)).asInstanceOf[Year].getValue,
            validations(yd.validations)
          )
      case ym: YearMonthData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("string value representing the year/month"),
            example.getOrElse(YearMonth.of(2020, Month.JANUARY)).toString,
            validations(ym.validations)
          )
      case z: ZoneIdData =>
        name =>
          addEnumerationData(
            name,
            description.getOrElse("string value representing the zone id offset"),
            ZoneId.systemDefault.toString,
            ZoneId.getAvailableZoneIds.asScala.toList,
            validations(z.validations)
          )
      case z: ZonedDateTimeData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("string value representing a zoned date time"),
            zonedDateTimeFormatter.format(instantExample),
            validations(z.validations)
          )
      case z: ZoneOffsetData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("string value representing a zone offset"),
            example.getOrElse(ZoneOffset.UTC).toString,
            validations(z.validations)
          )
    }
}
