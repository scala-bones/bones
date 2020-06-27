package com.bones.interpreter.values

import java.util.UUID

import cats.data.NonEmptyList
import com.bones.Util.stringToUuid
import com.bones.data.Error
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.interpreter.{InterchangeFormatValidator, KvpInterchangeFormatValidatorInterpreter}

trait JavaUtilValidator[IN] extends InterchangeFormatValidator[JavaUtilValue, IN] {

  val baseValidator: KvpInterchangeFormatValidatorInterpreter[IN]

  override def validate[A](alg: JavaUtilValue[A]):
    (Option[IN], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    alg match {
      case UuidData(validations) => {
        baseValidator.required(
          Right(alg),
          validations,
          (in, path) => baseValidator.extractString(alg, classOf[UUID])(in, path).flatMap(stringToUuid(_, path))
        )
      }
    }
  }
}
