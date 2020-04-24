package com.bones.interpreter.custom

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.Error.RequiredValue
import com.bones.data.custom.CustomStringValue
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.validation.ValidationUtil

trait CustomStringValidator[OUT] extends InterchangeFormatValidator[CustomStringValue, OUT] {

  val baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT]

  override def validate[A](alg: CustomStringValue[A])
    : (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (in: Option[OUT], path: List[String]) =>
      {
        in match {
          case Some(json) =>
            baseValidator
              .extractString(Right(alg), classOf[String])(json, path)
              .flatMap(result => {
                val allValidations = alg.customValidation :: alg.validations
                ValidationUtil
                  .validate[String](allValidations)(result, path)
                  .asInstanceOf[Either[NonEmptyList[Error.ExtractionError], A]]
              })
          case None =>
            Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
        }
      }
  }
}
