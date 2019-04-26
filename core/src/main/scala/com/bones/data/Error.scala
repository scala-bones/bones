package com.bones.data

import cats.data.NonEmptyList
import com.bones.data.Value.ValueDefinitionOp
import com.bones.validation.ValidationDefinition.ValidationOp

object Error {
  /** Error Case */
  sealed trait ExtractionError {
    def path: List[String]
  }

  /**
    * Used to indicate a validations error.
    * @param failurePoint The extraction from where the error failed.
    * @param input The input, if available.
    * @tparam T Type that was validated.
    */
  case class ValidationError[T](path: List[String], failurePoint: ValidationOp[T], input: T) extends ExtractionError
  case class WrongTypeError[T](path: List[String], expectedType: Class[T], providedType: Class[_]) extends ExtractionError
  case class CanNotConvert[A,T](path: List[String], input: A, toType: Class[T]) extends ExtractionError
  case class RequiredData[A](path: List[String], zvalueDefinitionOp: ValueDefinitionOp[A]) extends ExtractionError
  case class FieldError[A](path: List[String], key: String, errors: NonEmptyList[ExtractionError]) extends ExtractionError
  case class ParsingError[A](message: String) extends ExtractionError {
    override def path: List[String] = List.empty
  }
  case class SystemError(path: List[String], th: Throwable, message: Option[String]) extends ExtractionError
  case class NotFound[ID](id: ID, entityName: String, path: List[String]) extends ExtractionError

}
