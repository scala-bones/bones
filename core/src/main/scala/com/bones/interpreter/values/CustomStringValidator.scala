package com.bones.interpreter.values

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.Error.RequiredValue
import com.bones.data.values.CustomStringValue
import com.bones.interpreter.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  KvpInterchangeFormatValidatorInterpreter
}
import com.bones.validation.ValidationUtil

trait CustomStringValidator[IN] extends InterchangeFormatValidatorValue[CustomStringValue, IN] {

  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def validate[A](alg: CustomStringValue[A])
    : (Option[IN], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (in: Option[IN], path: List[String]) =>
      {
        in match {
          case Some(json) =>
            baseValidator
              .extractString(alg, classOf[String])(json, path)
              .flatMap(result => {
                val allValidations = alg.customValidation :: alg.validations
                ValidationUtil
                  .validate[String](allValidations)(result, path)
                  .asInstanceOf[Either[NonEmptyList[Error.ExtractionError], A]]
              })
          case None =>
            Left(NonEmptyList.one(RequiredValue.fromDef(path, Right(alg))))
        }
      }
  }
}
