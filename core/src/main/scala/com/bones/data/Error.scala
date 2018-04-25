package com.bones.data

import cats.data.NonEmptyList
import com.bones.data.Algebra.DataDefinitionOp
import com.bones.validation.ValidationDefinition.ValidationOp

object Error {
  /** Error Case */
  sealed trait ExtractionError

  /**
    * Used to indicate a validation error.
    * @param failurePoint The extraction op where the error failed.
    * @param input The input, if available.
    * @tparam T Type that was validated.
    */
  case class ValidationError[T](failurePoint: ValidationOp[T], input: T) extends ExtractionError
  case class WrongTypeError[T](expectedType: Class[T], providedType: Class[_]) extends ExtractionError
  case class CanNotConvert[A,T](input: A, toType: Class[T]) extends ExtractionError
  case class RequiredData[A](dataDefinitionOp: DataDefinitionOp[A]) extends ExtractionError
  case class FieldError[A](key: Key, errors: NonEmptyList[ExtractionError]) extends ExtractionError

}
