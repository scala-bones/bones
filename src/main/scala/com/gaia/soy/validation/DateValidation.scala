package com.gaia.soy.validation

import java.text.{DateFormat, SimpleDateFormat}
import java.time.format.DateTimeFormatter
import java.util.Date

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import com.gaia.soy.{ExtractionError, ExtractionErrors, ExtractionOp, FieldGroupOp, JsonProducer, Key, ValidationError}

import scala.reflect.macros.ParseException

trait DateValidation {
  import com.gaia.soy.StringValidation._

  case class IsDate(format: DateFormat) extends ExtractionOp[Date] {
    override def description: String = s"Is a Date with format ${format.toString}"
  }

  val format = DateTimeFormatter.ISO_DATE

  def convert(input: String, format: DateFormat, key: => Key): Validated[ExtractionErrors, Date] = try {
    Valid(format.parse(input))
  } catch {
    case _: ParseException => Invalid(NonEmptyList.one(ValidationError(key, IsDate(format), Some(input))))
  }


  case class RequiredDateExtraction(stringExtraction: RequiredString, dateFormat: DateFormat) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], Date]] {

    override def extract(jsonProducer: JsonProducer): Validated[ExtractionErrors, Date] =
      stringExtraction.extract(jsonProducer).andThen(convert(_, dateFormat, stringExtraction.key))
  }

  case class OptionalDateExtraction(stringExtraction: OptionalString, dateFormat: DateFormat) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], Option[Date]]] {
    override def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Option[Date]] =
      stringExtraction.extract(producer).andThen {
        case Some(uuidStr) => convert(uuidStr, dateFormat, stringExtraction.key).map(Some(_))
        case None => Valid(None)
      }
  }

  trait DateDefinitions[T] {
    def asDate(format: DateFormat): T
    def asIsoDateTime() = asDate(DateTimeFormatter.ISO_DATE_TIME.toFormat.asInstanceOf[DateFormat])
    def asIsoDate() = asDate(DateTimeFormatter.ISO_DATE.toFormat.asInstanceOf[DateFormat])
  }

  implicit class OptionalStringToDate(optionalString: OptionalString) extends DateDefinitions[OptionalDateExtraction] {
    def asDate(format: DateFormat) : OptionalDateExtraction = OptionalDateExtraction(optionalString, format)
  }

  implicit class RequiredStringToDate(requiredString: RequiredString) extends DateDefinitions[RequiredDateExtraction] {
    def asDate(format: DateFormat) : RequiredDateExtraction = RequiredDateExtraction(requiredString, format)
  }
}
