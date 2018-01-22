package com.ot.bones.validation

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, RequiredObjectError, ValidateFromProducer, ValidationResultNel}

object EitherValidation {

  final case class RequiredEither[A,B](key: Key, definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B])
    extends DataDefinitionOp[Either[A,B]] {

    def extract(producer:JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[Either[A,B]] = {

      functionK.apply(definitionA).apply(producer).map(Left(_))
        .orElse(functionK.apply(definitionB).apply(producer).map(Right(_)))
    }

  }

}
