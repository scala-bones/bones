package com.ot.bones.validation

import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.interpreter.ExtractionInterpreter.{BoolProducer, ExtractionErrors, RequiredObjectError}

object BooleanValidation {

  final case class RequiredBoolean(key: Key) extends DataDefinitionOp[Boolean] {

    def extract(producer: BoolProducer): Validated[ExtractionErrors, Boolean] = {
      producer.produceBool(key).leftMap(NonEmptyList.one) andThen {
        case Some(x) => Valid(x)
        case None => Invalid(NonEmptyList.one(RequiredObjectError(key)))
      }
    }

    def optional: OptionalBoolean = OptionalBoolean(key)
  }

  final case class OptionalBoolean(key: Key) extends DataDefinitionOp[Option[Boolean]] {

    def extract(producer: BoolProducer) : Validated[ExtractionErrors, Option[Boolean]] = {
      producer.produceBool(key).leftMap(NonEmptyList.one)
    }
  }

}
