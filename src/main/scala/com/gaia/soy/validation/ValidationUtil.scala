package com.gaia.soy.validation

import cats.Apply
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}
import com.gaia.soy.{Key, ValidationError, ValidationOp, ValidationResultNel}

object ValidationUtil {

  def runValidation[T](key: Key, input: T, validation: ValidationOp[T]): Validated[ValidationError[T, T], T] = {
    if (validation.isValid(input)) {
      Valid(input)
    } else {
      Invalid(ValidationError(key, validation, Some(input)))
    }
  }

  /**
    * Responsible for running the list of validations.
    */
  def runAndMapValidations[T](key: Key, input: T, validations: List[ValidationOp[T]])(implicit A: Apply[ValidatedNel[ValidationError[T,T], ?]]): ValidationResultNel[T] =
    validations.map(ValidationUtil.runValidation(key, input, _))
      .foldLeft[ValidatedNel[ValidationError[T, T], T]](Valid(input))((last, next) => {
      val n: ValidatedNel[ValidationError[T, T], T] = next.leftMap(NonEmptyList.one)
      A.map2(last, n)((a, b) => b)
    })

}
