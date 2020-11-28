package com.bones.swagger.values

import java.time._
import java.time.format.DateTimeFormatter

import com.bones.data.values._
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter
import com.bones.validation.ValidationDefinition.ValidationOp
import io.swagger.v3.oas.models.media.Schema

//import scala.jdk.CollectionConverters._ //because cross compile 2.12
import scala.collection.JavaConverters._

trait JavaTimeInterpreter extends CustomSwaggerInterpreter[JavaTimeValue] {

  import SwaggerCoreInterpreter._

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val localDateTimeFormatter: DateTimeFormatter
  val localDateFormatter: DateTimeFormatter
  val localTimeFormatter: DateTimeFormatter

  val exception = new DateTimeException("The error message for a DateTimeException")

  // TODO add validation descriptions
  def validations[A](list: List[ValidationOp[A]]): Schema[_] => Schema[_] = identity

  override def toSchema[A](
    alg: JavaTimeValue[A],
    description: Option[String],
    example: Option[A]): Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] = {

    val swaggerDescription =
      description.getOrElse(JavaTimeValueDefaultJsonMetadata.getDefaultDescription(alg))

    val swaggerExample =
      example.getOrElse(JavaTimeValueDefaultJsonMetadata.getDefaultExample(alg))

    alg match {
      case dte: DateTimeExceptionData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[DateTimeException].getMessage,
            validations(dte.validations))
      case dow: DayOfWeekData =>
        name =>
          addEnumerationData(
            name,
            swaggerDescription,
            swaggerExample.toString,
            DayOfWeek.values.toList.map(_.toString),
            validations(dow.validations)
          )
      case d: DurationData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.toString,
            validations(d.validations)
          )
      case id: InstantData =>
        name =>
          addDateTimeSchema(
            name,
            swaggerDescription,
            instantFormatter.format(swaggerExample.asInstanceOf[Instant]),
            validations(id.validations)
          )
      case ldt: LocalDateTimeData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            localDateTimeFormatter.format(swaggerExample.asInstanceOf[LocalDateTime]),
            validations(ldt.validations)
          )
      case ld: LocalDateData =>
        name =>
          addDateSchema(
            name,
            swaggerDescription,
            localDateFormatter.format(swaggerExample.asInstanceOf[LocalDate]),
            validations(ld.validations)
          )
      case lt: LocalTimeData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            localTimeFormatter.format(swaggerExample.asInstanceOf[LocalTime]),
            validations(lt.validations)
          )
      case md: MonthData =>
        name =>
          addEnumerationData(
            name,
            swaggerDescription,
            swaggerExample.toString,
            Month.values.toList.map(_.toString),
            validations(md.validations)
          )
      case md: MonthDayData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.toString,
            validations(md.validations)
          )
      case od: OffsetDateTimeData =>
        name =>
          addDateTimeSchema(
            name,
            swaggerDescription,
            offsetDateTimeFormatter.format(swaggerExample.asInstanceOf[OffsetDateTime]),
            validations(od.validations)
          )
      case od: OffsetTimeData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            offsetTimeFormatter.format(swaggerExample.asInstanceOf[OffsetTime]),
            validations(od.validations)
          )
      case pd: PeriodData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.toString,
            validations(pd.validations)
          )
      case yd: YearData =>
        name =>
          addIntSchema(
            name,
            swaggerDescription,
            swaggerExample.asInstanceOf[Year].getValue,
            validations(yd.validations)
          )
      case ym: YearMonthData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.toString,
            validations(ym.validations)
          )
      case z: ZoneIdData =>
        name =>
          addEnumerationData(
            name,
            swaggerDescription,
            swaggerExample.toString,
            ZoneId.getAvailableZoneIds.asScala.toList,
            validations(z.validations)
          )
      case z: ZonedDateTimeData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            zonedDateTimeFormatter.format(swaggerExample.asInstanceOf[ZonedDateTime]),
            validations(z.validations)
          )
      case z: ZoneOffsetData =>
        name =>
          addStringSchema(
            name,
            swaggerDescription,
            swaggerExample.toString,
            validations(z.validations)
          )
    }
  }
}
