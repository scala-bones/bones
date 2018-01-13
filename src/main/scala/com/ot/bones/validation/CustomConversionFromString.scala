package com.ot.bones.validation

import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated}
import com.ot.bones.BonesOp
import com.ot.bones.compiler.ExtractionCompiler.{ExtractionError, ExtractionOp, JsonProducer, ValidationError}
import com.ot.bones.validation.StringValidation.{OptionalString, RequiredString}

object CustomConversionFromString {
  import StringValidation._
  case class CustomExtractionOp[T](description: String) extends ExtractionOp[T]

  case class RequiredCustomExtraction[T](requiredString: RequiredString, description: String, f: String => Validated[String,T])
    extends BonesOp[T] {
    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], T] =
      requiredString.extract(producer).andThen(res => f(res)
        .leftMap(err => NonEmptyList.one(ValidationError(requiredString.key, CustomExtractionOp(description), Some(res)))))
  }

  case class OptionalCustomExtraction[T](optionalString: OptionalString, description: String, f: String => Validated[String,T])
    extends BonesOp[Option[T]] {

    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Option[T]] =
      optionalString.extract(producer).andThen {
        case Some(customStr) => {
          f(customStr).map(res => Some(res)).leftMap(err => NonEmptyList.one(ValidationError(optionalString.key, CustomExtractionOp(description), Some(customStr))))
        }
        case None => Valid(None)
      }

  }


}

/** Defines a custom conversion for optional and required results.  Requires a String => Validated[String,T] function and a description. */
trait CustomConversionFromString {

  import CustomConversionFromString._

  implicit class OptionalCustomConversionFromString(optionalString: OptionalString) {
    def custom[T](description: String, f: String => Validated[String,T]) : OptionalCustomExtraction[T] = OptionalCustomExtraction(optionalString,description,f)
  }

  implicit class RequiredCustomConversionFromString(requiredString: RequiredString) {
    def custom[T](description: String, f: String => Validated[String,T]) : RequiredCustomExtraction[T] = RequiredCustomExtraction(requiredString, description,f)
  }

}

