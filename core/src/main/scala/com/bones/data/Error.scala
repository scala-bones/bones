package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp

/**
  * Various Errors for use in Bones interpreters.
  */
object Error {

  type ExtractionErrors[K] = List[ExtractionError[K]]

  /** Error Case */
  sealed abstract class ExtractionError[K] {
    def path: List[K]
  }

  /**
    * Used to indicate a validations error.
    *
    * @param failurePoint The extraction from where the error failed.
    * @param input        The input, if available.
    * @tparam T Type that was validated.
    */
  final case class ValidationError[K, T](path: List[K], failurePoint: ValidationOp[T], input: T)
      extends ExtractionError[K]

  /** Used when we receive a type that doesn't match the expected type.
    * For instance if we are expecting an Int, but we receive a String.
    *
    * @param path         The path within the schema to the offending definition
    * @param expectedType The expected type.
    * @param providedType What was actually provided.
    * @tparam T The expected type
    */
  final case class WrongTypeError[K, T](
    path: List[K],
    expectedType: String,
    providedType: String,
    cause: Option[Throwable])
      extends ExtractionError[K]

  /**
    * Used when we can not convert an input type to the type defined in the schema.
    * For instance, in JSON we use a String to represent an Enumeration.  However, if the
    * string does not map to a known enumeration, this error would be specified.
    *
    * @param path   The path within the schema to the offending definition
    * @param input  The input type being converted
    * @param toType The type we are attempting to convert to.
    * @tparam A Input type
    * @tparam T Type to convert to
    */
  final case class CanNotConvert[K, A, T](
    path: List[K],
    input: A,
    toType: Class[T],
    cause: Option[Throwable])
      extends ExtractionError[K]

  /** Used when a required piece of data is missing
    *
    * @param path                    The path within the schema to the offending definition
    * @param typeName The description of the required value
    */
  final case class RequiredValue[K](path: List[K], typeName: String) extends ExtractionError[K]

  final case class SumTypeError[K](path: List[K], problem: String) extends ExtractionError[K]

  /**
    * Used when we can not parse the input to its expected format -- for instance, an invalid JSON document.
    *
    * @param message   Message from the parser.
    * @param throwable Exception from the parser.
    */
  final case class ParsingError[K](message: String, throwable: Option[Throwable] = None)
      extends ExtractionError[K] {
    override def path: List[K] = List.empty
  }

  /**
    * Used when an exception is thrown in processing the request in the business logic.
    * This is a reasonable exception to use as starting point for Exceptions reported in the integration layer.
    *
    * @param th      The Exception.
    * @param message Context specific message about the exception.
    */
  final case class SystemError[K](path: List[K], th: Throwable, message: Option[String])
      extends ExtractionError[K]

  object SystemError {

    /** Alias for SystemError without a path */
    def apply[K](th: Throwable, message: Option[String]): SystemError[K] =
      SystemError(List.empty, th, message)
  }

  /**
    * Used when the ID is not present in the integration layer.
    *
    * @param id         The id of the entity.
    * @param entityName The name of the entity.
    * @param path       The path to the entity
    * @tparam ID The type of the ID (such as Long or UUID)
    */
  final case class NotFound[K, ID](id: ID, entityName: String, path: List[K])
      extends ExtractionError[K]

}
