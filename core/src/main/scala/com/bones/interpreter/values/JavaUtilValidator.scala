package com.bones.interpreter.values

import com.bones.Util.stringToUuid
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  OptionalInputValidator
}

trait JavaUtilValidator[IN] extends InterchangeFormatValidatorValue[JavaUtilValue, IN] {

  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def createValidator[A](
    alg: JavaUtilValue[A]
  ): OptionalInputValidator[String, JavaUtilValue, A, IN] = {
    alg match {
      case UuidData(validations) => {
        baseValidator.required(
          alg.typeName,
          validations,
          (in, path) =>
            baseValidator
              .extractString(alg.typeName)
              .validateWithPath(in, path)
              .flatMap(stringToUuid(_, path))
              .asInstanceOf[Either[ExtractionErrors[String], A]]
        )
      }
    }
  }
}
