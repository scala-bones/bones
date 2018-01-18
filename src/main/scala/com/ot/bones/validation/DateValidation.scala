package com.ot.bones.validation

import java.text.Format
import java.time.format.DateTimeFormatter
import java.util.Date

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.ot.bones.interpreter.ExtractionInterpreter.{AppendGenericValidation, ExtractionError, ExtractionErrors, ExtractionOp, JsonProducer, ValidationError, ValidationOp}
import com.ot.bones.validation.StringValidation.{OptionalString, RequiredString}

import scala.reflect.macros.ParseException

object DateValidation {
  case class IsDate(format: Format, formatDescription: Option[String]) extends ExtractionOp[Date] {
    override def description: String = formatDescription.getOrElse(s"Is a Date with format ${format.toString}")
  }

  def convert(input: String, format: Format, formatDescription: Option[String], key: => Key): Validated[ExtractionErrors, Date] = try {
    Valid(format.parseObject(input).asInstanceOf[Date])
  } catch {
    case _: ParseException => Invalid(NonEmptyList.one(ValidationError(key, IsDate(format, formatDescription), Some(input))))
  }

  case class Min(minDate: Date, format: Format) extends ValidationOp[Date] {
    override def isValid: Date => Boolean = _.after(minDate)

    override def defaultError(t: Date): String = s"specified date ${format.format(t)} must be after ${format.format(minDate)}"

    override def description: String = s"after ${format.format(minDate)}"
  }

  case class Max(maxDate: Date, format: Format) extends ValidationOp[Date] {
    override def isValid: Date => Boolean = _.before(maxDate)

    override def defaultError(t: Date): String = s"specified date ${format.format(t)} must be before ${format.format(maxDate)}"

    override def description: String = s"before ${format.format(maxDate)}"
  }


  trait BaseDateValidation[F[_], NSE] extends AppendGenericValidation[Date, F, NSE] {

    val dateFormat: Format

    /** Add the validation enforcing that the supplied value must be greater than min */
    def min(min: Date): NSE = append(Min(min, dateFormat))
    /** Add the validation enforcing that the supplied value must be less than max */
    def max(max: Date): NSE = append(Max(max, dateFormat))

  }


  case class RequiredDateExtraction(stringExtraction: RequiredString, dateFormat: Format, formatDescription: Option[String], validations: List[ValidationOp[Date]])
    extends DataDefinitionOp[Date] {

    def extract(jsonProducer: JsonProducer): Validated[ExtractionErrors, Date] =
      stringExtraction.extract(jsonProducer).andThen(convert(_, dateFormat, formatDescription, stringExtraction.key))
  }

  case class OptionalDateExtraction(stringExtraction: OptionalString, dateFormat: Format, formatDescription: Option[String], validations: List[ValidationOp[Date]]) extends DataDefinitionOp[Option[Date]] {
    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Option[Date]] =
      stringExtraction.extract(producer).andThen {
        case Some(uuidStr) => convert(uuidStr, dateFormat, formatDescription, stringExtraction.key).map(Some(_))
        case None => Valid(None)
      }
  }
}

/** Implicits in order to add data validation to a String */
trait DateValidation {
  import DateValidation._
  import StringValidation._

  trait DateDefinitions[T] {
    /** Specify the format and the description.  If None, then we use the format.toString as a description.*/
    def date(format: Format, formatDescription: Option[String] = None): T
    /** Must be ISO Date Time format */
    def isoDateTime() =
      date(
        DateTimeFormatter.ISO_DATE_TIME.toFormat,
        Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'")
      )

    /** Must be ISO Date format */
    def isoDate() = date(DateTimeFormatter.ISO_DATE.toFormat, Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"))
  }

  implicit class OptionalStringToDate(optionalString: OptionalString) extends DateDefinitions[OptionalDateExtraction] {
    def date(format: Format, formatDescription: Option[String] = None) : OptionalDateExtraction = OptionalDateExtraction(optionalString, format, formatDescription, List.empty)
  }

  implicit class RequiredStringToDate(requiredString: RequiredString) extends DateDefinitions[RequiredDateExtraction] {
    def date(format: Format, formatDescription: Option[String] = None) : RequiredDateExtraction = RequiredDateExtraction(requiredString, format, formatDescription, List.empty)
  }
}
