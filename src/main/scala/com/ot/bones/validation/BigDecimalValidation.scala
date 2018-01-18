package com.ot.bones.validation

import cats.Id
import cats.arrow.FunctionK
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.interpreter.ExtractionInterpreter.{AppendGenericValidation, BigDecimalProducer, ConversionError, ExtractionErrors, JsonProducer, RequiredObjectError, StringProducer, ValidateFromProducer, ValidationOp, WrongTypeError}
import com.ot.bones.validation.StringValidation.{OptionalString, RequiredString}
import cats.implicits._

object BigDecimalValidation {

  case class Max(max: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd => max >= inputBd

    override def defaultError(t: BigDecimal): String = s"$t is greater than the maximum $max"

    override def description: String = s"maximum value of ${max.toString()}"
  }

  case class Min(min: BigDecimal) extends ValidationOp[BigDecimal] {
    override def isValid: (BigDecimal) => Boolean = inputBd => min <= inputBd

    override def defaultError(t: BigDecimal): String = s"$t is less than the minimum $min}"

    override def description: String = s"minimum value of $min.toString()"
  }

  trait BigDecimalExtraction[F[_], NSE] extends AppendGenericValidation[BigDecimal, F, NSE] {

    /** Add the validation enforcing that the supplied value must be greater than min */
    def min(min: Int): NSE = append(Min(min))
    /** Add the validation enforcing that the supplied value must be less than max */
    def max(max: Int): NSE = append(Max(max))

    val validations: List[ValidationOp[BigDecimal]]

    def extractFromStringAndValidate(str: String, key: Key) : Validated[ExtractionErrors, BigDecimal] = {
      val converted = try {
        Valid(BigDecimal(str))
      } catch {
        case ex: NumberFormatException => Invalid(NonEmptyList.one(ConversionError(key, str, classOf[BigDecimal])))
      }

      converted.andThen(bigDecimal => {
        ValidationUtil.runAndMapValidations(key, bigDecimal, validations)
      })

    }

  }

  final case class OptionalBigDecimal(keyOrOptionalString: Either[Key, OptionalString], validations: List[ValidationOp[BigDecimal]])
    extends DataDefinitionOp[Option[BigDecimal]]
    with BigDecimalExtraction[Option, OptionalBigDecimal] {

    override def append(sv: ValidationOp[BigDecimal]): OptionalBigDecimal =
      OptionalBigDecimal(keyOrOptionalString, sv :: validations)

    def extract(input: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, Option[BigDecimal]] = {
      keyOrOptionalString match {
        case Left(key) => input.produceString(key).leftMap(NonEmptyList.one(_)).toValidated andThen {
          case Some(str) => extractFromStringAndValidate(str, key).map(Some(_))
          case None => Valid(None)
        }
        case Right(optionalString) => {
          functionK(optionalString)(input) andThen {
            case Some(str) => extractFromStringAndValidate(str, optionalString.key).map(Some(_))
            case None => Valid(None)
          }
        }
      }
    }
  }

  final case class RequiredBigDecimal(keyOrRequiredString: Either[Key, RequiredString], validations: List[ValidationOp[BigDecimal]])
    extends DataDefinitionOp[BigDecimal]
    with BigDecimalExtraction[Id, RequiredBigDecimal] {


    override def append(sv: ValidationOp[BigDecimal]): RequiredBigDecimal = RequiredBigDecimal(keyOrRequiredString, sv :: validations)

    def extract(input: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, BigDecimal] = {
      keyOrRequiredString match {
        case Left(key) => input.produceString(key).leftMap(NonEmptyList.one(_)).toValidated andThen {
          case Some(str) => extractFromStringAndValidate(str, key)
          case None => Invalid(NonEmptyList.one(RequiredObjectError(key)))
        }
        case Right(optionalString) => {
          functionK(optionalString)(input) andThen { str =>
            extractFromStringAndValidate(str, optionalString.key)
          }
        }
      }
    }
  }

}
