package com.bones.interpreter.deltavalidator

import com.bones.Util.{CanBeOmitted, stringToUuid}
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.validation.ValidationUtil

import java.util.UUID

trait JavaUtilValueInterpreter[IN] extends InterchangeFormatDeltaValidatorValue[JavaUtilValue, IN] {
  val primitive: PrimitiveInterchangeFormat[IN, String]

  override def createDeltaValidator[A](
    alg: JavaUtilValue[A]
  ): DeltaValueValidator[String, JavaUtilValue, A, IN] =
    alg match {
      case _: UuidData =>
        primitive
          .extractString("String")
          .flatMapA[UUID](str =>
            new DeltaValueValidator[String, JavaUtilValue, A, IN] {
              override def extract(in: IN, key: String, path: List[String])
                : Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
                stringToUuid(str, path).map(Right(_))
            }
          )
          .addValidation(ValidationUtil.validate(alg.validations))
          .asInstanceOf[DeltaValueValidator[String, JavaUtilValue, A, IN]]
    }

}
