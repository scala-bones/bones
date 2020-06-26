package com.bones.interpreter.custom

import java.util.UUID

import cats.data.NonEmptyList
import com.bones.Util.stringToUuid
import com.bones.data.Error
import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator

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
