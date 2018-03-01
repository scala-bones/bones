package com.ot.bones.convert

import java.text.{DateFormat, Format}
import java.time.format.DateTimeFormatter
import java.util.{Date, UUID}

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.free.FreeApplicative
import com.ot.bones.DataConversion
import com.ot.bones.StringDataDefinition.{OptionalString, RequiredString}
import com.ot.bones.convert.string.StringConversionOp
import com.ot.bones.interpreter.ExtractionInterpreter.{ConversionError, ExtractionError, ExtractionErrors, ValidationError}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import com.ot.bones.validation.{DataDefinitionOp, Key, ValidationUtil}

import scala.reflect.macros.ParseException

package object string {

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition..*/
  sealed trait StringConversionOp[A] {
    //lift any DataDefinition into a FreeApplicative
    def lift: StringConversion[A] = FreeApplicative.lift(this)
  }

  type StringConversion[A] = FreeApplicative[StringConversionOp, A]

  implicit class DataOpConversion( k: (Key, DataDefinitionOp[String])) {
    def as[B](op: StringConversionOp[B]): (Key, StringConversionOp[B]) = ???
  }


}

object BigDecimalValidation {

  case class Max(max: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd => max >= inputBd

    override def defaultError(t: BigDecimal): String = s"$t is greater than the maximum $max"

    override def description: String = s"maximum value of ${max.toString()}"
  }

  case class Min(min: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd => min <= inputBd

    override def defaultError(t: BigDecimal): String = s"$t is less than the minimum $min"

    override def description: String = s"minimum value of $min.toString()"
  }

  final case class ConvertBigDecimal(validations: List[ValidationOp[BigDecimal]])
    extends StringConversionOp[Option[BigDecimal]] {

    /** Add the validation enforcing that the supplied value must be greater than min */
    def min(min: Int): ConvertBigDecimal = ConvertBigDecimal(Min(min) :: validations)
    /** Add the validation enforcing that the supplied value must be less than max */
    def max(max: Int): ConvertBigDecimal = ConvertBigDecimal(Max(max) :: validations)

    def convertFromStringAndValidate(str: String) : Validated[ExtractionErrors, BigDecimal] = {
      val converted = try {
        Valid(BigDecimal(str))
      } catch {
        case ex: NumberFormatException => Invalid(NonEmptyList.one(ConversionError(str, classOf[BigDecimal])))
      }

      converted.andThen(bigDecimal => {
        ValidationUtil.runAndMapValidations(bigDecimal, validations)
      })

    }

  }

}


object CustomConversionFromString {

  case class CustomConversion[D[_], I, T:Manifest](description: String, f: I => Validated[String,T])
    extends StringConversionOp[T] {

    def convert(i: I): Validated[NonEmptyList[ExtractionError], T] = {
      f(i).leftMap(e => NonEmptyList.one(ConversionError(i, manifest[T].runtimeClass)))
    }
  }

}

/** Defines a custom conversion for optional and required results.  Requires a String => Validated[String,T] function and a description. */
trait CustomConversionFromStringInstances {

  implicit class DataOpToCustomConversionFromString[A, D <: DataDefinitionOp[A]](dataDefinitionOp: D) {
    def custom[T:Manifest](description: String, f: A => Validated[String, T]): DataConversion[A, D, StringConversionOp, T] =
      DataConversion(dataDefinitionOp, CustomConversionFromString.CustomConversion(description, f))
  }

}

object DateConversionInstances {

  implicit class DataOpConversion(dataOp: (Key, DataDefinitionOp[String])) {
    def as[T](conversion: StringConversionOp[T]) : (Key, StringConversionOp[T])  = ???
  }

  case class IsDate(format: Format, formatDescription: Option[String]) extends StringConversionOp[Date] {
    def description: String = formatDescription.getOrElse(s"Is a Date with format ${format.toString}")
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


  case class DateConversion(dateFormat: Format, formatDescription: Option[String], validations: List[ValidationOp[Date]])
    extends StringConversionOp[Date] {

    /** Add the validation enforcing that the supplied value must be greater than min */
    def min(min: Date): DateConversion = DateConversion(dateFormat, formatDescription, Min(min, dateFormat) :: validations)
    /** Add the validation enforcing that the supplied value must be less than max */
    def max(max: Date): DateConversion = DateConversion(dateFormat, formatDescription, Max(max, dateFormat) :: validations)

    def convert(str: String): Validated[ExtractionErrors, Date] =  try {
      Valid(dateFormat.parseObject(str).asInstanceOf[Date])
    } catch {
      case _: ParseException => Invalid(NonEmptyList.one(ConversionError(str, classOf[Date])))
    }

  }

}

/** Implicits in order to add data validation to a String */
trait DateConversionInstances {

  import DateConversionInstances._

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

  implicit class RequiredStringToDate(rs: RequiredString) {
    /** Date, BYOFormat */
    def date(dateFormat: DateFormat, formatDescription: Option[String] = None): DateConversion =
      DateConversion(dateFormat, formatDescription, List.empty)

    /** Expecting a string that is in the format of an iso date time */
    def isoDateTime(): DataConversion[String, RequiredString, StringConversionOp, Date] =
      DataConversion(rs, DateConversion(
        DateTimeFormatter.ISO_DATE_TIME.toFormat,
        Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'"),
        List.empty
      ))

    /** Expecting a string that is in the format of an iso date */
    def isoDate() =
      DateConversion(
        DateTimeFormatter.ISO_DATE.toFormat,
        Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"),
        List.empty
      )
  }

  implicit class OptionalStringToDate(os: OptionalString) {
    /** Date, BYOFormat */
    def date(dateFormat: DateFormat, formatDescription: Option[String] = None): DateConversion =
      DateConversion(dateFormat, formatDescription, List.empty)

    /** Expecting a string that is in the format of an iso date time */
    def isoDateTime(): DateConversion =
      DateConversion(
        DateTimeFormatter.ISO_DATE_TIME.toFormat,
        Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'"),
        List.empty
      )

    /** Expecting a string that is in the format of an iso date */
    def isoDate() =
      DateConversion(
        DateTimeFormatter.ISO_DATE.toFormat,
        Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"),
        List.empty
      )
  }


}


object UuidConversionInstances {

  object IsUuid extends StringConversionOp[UUID] {
    def description: String = "Is a UUID"
  }

  final case class UuidConversion() extends StringConversionOp[UUID] {

    def convert(uuidString: String): Validated[ExtractionErrors, UUID] = try {
      Valid(UUID.fromString(uuidString))
    } catch {
      case _: IllegalArgumentException => Invalid(NonEmptyList.one(ConversionError(uuidString, classOf[UUID])))
    }
  }

}

trait UuidConversionInstances {
  import com.ot.bones.StringDataDefinition._
  import UuidConversionInstances._

  implicit class OptionalStringToUuid(optionalString: OptionalString) {
    def asUuid() : DataConversion[Option[String], OptionalString, StringConversionOp, UUID] = DataConversion(optionalString,UuidConversion())
  }

  implicit class RequiredStringToUuid(requiredString: RequiredString) {
    def asUuid() : DataConversion[String, RequiredString, StringConversionOp, UUID] = DataConversion(requiredString, UuidConversion())
  }

}
