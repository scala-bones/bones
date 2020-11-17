package com.bones.interpreter.values

import com.bones.data.Error.{ExtractionErrors, RequiredValue}
import com.bones.data.values.CustomStringValue
import com.bones.interpreter.{InterchangeFormatPrimitiveValidator, InterchangeFormatValidatorValue}
import com.bones.validation.ValidationUtil

trait CustomStringValidator[IN] extends InterchangeFormatValidatorValue[CustomStringValue, IN] {

  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def validate[A](alg: CustomStringValue[A])
    : (Option[IN], List[String]) => Either[ExtractionErrors[String], A] = {
    (in: Option[IN], path: List[String]) =>
      {
        in match {
          case Some(json) =>
            baseValidator
              .extractString(alg.typeName)(json, path)
              .flatMap(result => {
                val allValidations = alg.customValidation :: alg.validations
                ValidationUtil
                  .validate(allValidations)(result, path)
                  .asInstanceOf[Either[ExtractionErrors[String], A]]
              })
          case None =>
            Left(List(RequiredValue(path, alg.typeName)))
        }
      }
  }
}
