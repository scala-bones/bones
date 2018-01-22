package com.ot.bones.validation

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, RequiredObjectError, ValidateFromProducer, ValidationResultNel}

object ListValidation {

  final case class RequiredList[T](key: Key, tDefinition: DataDefinitionOp[T]) extends DataDefinitionOp[List[T]] {

    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[List[T]] = {
      jsonProducer.produceList(key).leftMap(NonEmptyList.one).andThen {
        case Some(list) => {
          list.map(producer => {
            functionK(tDefinition)(producer)
          }).sequence[ValidationResultNel,T]
        }
        case None => Invalid(NonEmptyList.one(RequiredObjectError(key)))
      }

    }

    def optional() = OptionalList(key, tDefinition)

  }

  final case class OptionalList[T](key: Key, tDefinition: DataDefinitionOp[T]) extends DataDefinitionOp[Option[List[T]]] {
    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[Option[List[T]]] = {
      jsonProducer.produceList(key).leftMap(NonEmptyList.one).andThen {
        case Some(list) => {
          list.map(producer => {
            functionK(tDefinition)(producer)
          }).sequence[ValidationResultNel,T].map(Some(_))
        }
        case None => Valid(None)
      }

    }


  }



}
