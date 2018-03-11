package com.ot.bones.convert

import java.text.{DateFormat, Format, SimpleDateFormat}
import java.time.format.DateTimeFormatter
import java.util.{Date, UUID}

import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, ValidateFromProducer}
import com.ot.bones.validation.{RequiredFieldDefinition, ToOptionalData}
//import com.ot.bones.StringDataDefinition.OptionalString
import com.ot.bones.interpreter.ExtractionInterpreter.{ConversionError, ExtractionErrors, RequiredObjectError, StringProducer}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import com.ot.bones.validation.{DataDefinitionOp, FieldDefinition, Key}

import scala.reflect.macros.ParseException
import scala.util.control.NonFatal

package object string {

//  /** DataDefinitionOp is the base class defining the FreeAp for each data definition..*/
//  sealed trait StringConversionOp[B] extends Conversion[String,B] {
//    //lift any DataDefinition into a FreeApplicative
//    def lift: StringConversion[B] = FreeApplicative.lift(this)
//  }
//
//  type StringConversion[B] = FreeApplicative[StringConversionOp, B]
//
//  implicit class DataOpConversion( k: (Key, DataDefinitionOp[String])) {
//    def as[B](op: StringConversionOp[B]): (Key, StringConversionOp[B]) = ???
//  }


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

    override def description: String = s"minimum value of ${min}"
  }

  final case class BigDecimalFromString() extends DataDefinitionOp[BigDecimal] {

    def extract(stringProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : Validated[ExtractionErrors, BigDecimal] = {
      stringProducer.produceString.leftMap(NonEmptyList.one).andThen {
        case Some(str) => convertFromString(str)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }

    def convertFromString(str: String) : Validated[ExtractionErrors, BigDecimal] = {
      try {
        Valid(BigDecimal(str))
      } catch {
        case ex: NumberFormatException => Invalid(NonEmptyList.one(ConversionError(str, classOf[BigDecimal])))
      }
    }
  }

}


object DateConversionInstances {

  case class IsDate(format: Format, formatDescription: Option[String]) {
    def description: String = formatDescription.getOrElse(s"Is a Date with format ${format.toString}")
  }

  case class Min(minDate: Date, format: SimpleDateFormat) extends ValidationOp[Date] {
    override def isValid: Date => Boolean = _.after(minDate)

    override def defaultError(t: Date): String = s"specified date ${format.format(t)} must be after ${format.format(minDate)}"

    override def description: String = s"after ${format.format(minDate)}"
  }

  case class Max(maxDate: Date, format: SimpleDateFormat) extends ValidationOp[Date] {
    override def isValid: Date => Boolean = _.before(maxDate)

    override def defaultError(t: Date): String = s"specified date ${format.format(t)} must be before ${format.format(maxDate)}"

    override def description: String = s"before ${format.format(maxDate)}"
  }


  case class DateConversion(dateFormat: SimpleDateFormat, formatDescription: Option[String], validations: List[ValidationOp[Date]]) {

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

  case class DateData(dateFormat: DateFormat, formatDescription: Option[String]) extends DataDefinitionOp[Date] with ToOptionalData[Date] {

    def extract(stringProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, Date] = {
      stringProducer.produceString.leftMap(NonEmptyList.one).andThen {
        case Some(str) => try {
          Valid(dateFormat.parseObject(str).asInstanceOf[Date])
        } catch {
          case NonFatal(ex) => Invalid(NonEmptyList.one(ConversionError(str, classOf[Date])))
        }
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }
  }

  implicit class DateExtensions(key: Key) {

    /** Date, BYOFormat */
    def date(dateFormat: DateFormat, formatDescription: Option[String] = None): FieldDefinition[Date, DateData] =
      RequiredFieldDefinition(key, DateData(dateFormat, formatDescription), List.empty)

    /** Expecting a string that is in the format of an iso date time */
    def isoDateTime(): RequiredFieldDefinition[Date, DateData] =
      RequiredFieldDefinition(key,
        DateData(
          new SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'"),
          Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'")
        ),
        List.empty
      )

    /** Expecting a string that is in the format of an iso date */
    def isoDate() =
      DateConversion(
        new SimpleDateFormat("yyyy-MM-dd"),
        Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"),
        List.empty
      )
  }

}


object UuidConversionInstances {

  def convert(uuidString: String): Validated[ExtractionErrors, UUID] = try {
    Valid(UUID.fromString(uuidString))
  } catch {
    case _: IllegalArgumentException => Invalid(NonEmptyList.one(ConversionError(uuidString, classOf[UUID])))
  }


  final case class UuidData() extends DataDefinitionOp[UUID] with ToOptionalData[UUID] {

    def extract(stringProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : Validated[ExtractionErrors, UUID] = {
      stringProducer.produceString.leftMap(NonEmptyList.one).andThen {
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
        case Some(str) => convert(str)
      }
    }

  }

}

trait UuidConversionInstances {
  import UuidConversionInstances._

  implicit class UuidExtensions(key: Key) {
    def uuid(): RequiredFieldDefinition[UUID, UuidData] = RequiredFieldDefinition(key, UuidData(), List.empty)
  }


}
