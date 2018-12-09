package com.bones.data

import cats.data.NonEmptyList
import com.bones.data.Value.ValueDefinitionOp
import com.bones.validation.ValidationDefinition.ValidationOp

object Error {
  /** Error Case */
  sealed trait ExtractionError {
    def path: Vector[String]
  }

  /**
    * Used to indicate a validations error.
    * @param failurePoint The extraction from where the error failed.
    * @param input The input, if available.
    * @tparam T Type that was validated.
    */
  case class ValidationError[T](path: Vector[String], failurePoint: ValidationOp[T], input: T) extends ExtractionError
  case class WrongTypeError[T](path: Vector[String], expectedType: Class[T], providedType: Class[_]) extends ExtractionError
  case class CanNotConvert[A,T](path: Vector[String], input: A, toType: Class[T]) extends ExtractionError
  case class RequiredData[A](path: Vector[String], zvalueDefinitionOp: ValueDefinitionOp[A]) extends ExtractionError
  case class FieldError[A](path: Vector[String], key: String, errors: NonEmptyList[ExtractionError]) extends ExtractionError

}
