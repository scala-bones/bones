package com.gaia.soy.validation

import java.util.UUID

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.gaia.soy.{ExtractionError, ExtractionErrors, ExtractionOp, FieldGroupOp, JsonProducer, Key, StringValidation, ValidationError}

trait UuidValidation {
  import StringValidation._

  object IsUuid extends ExtractionOp[UUID] {
    override def description: String = "Is a UUID"
  }

  def convert(uuidString: String, key: => Key): Validated[ExtractionErrors, UUID] = try {
    Valid(UUID.fromString(uuidString))
  } catch {
    case _: IllegalArgumentException => Invalid(NonEmptyList.one(ValidationError(key, IsUuid, Some(uuidString))))
  }


  case class RequiredUuidExtraction(stringExtraction: RequiredString) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], UUID]] {

    def extract(jsonProducer: JsonProducer): Validated[ExtractionErrors, UUID] =
      stringExtraction.extract(jsonProducer).andThen(convert(_,stringExtraction.key))
  }

  case class OptionalUuidExtraction(stringExtraction: OptionalString) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], Option[UUID]]] {
    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Option[UUID]] =
      stringExtraction.extract(producer).andThen {
        case Some(uuidStr) => convert(uuidStr, stringExtraction.key).map(Some(_))
        case None => Valid(None)
      }
  }

  implicit class OptionalStringToUuid(optionalString: OptionalString) {
    def asUuid() : OptionalUuidExtraction = OptionalUuidExtraction(optionalString)
  }

  implicit class RequiredStringToUuid(requiredString: RequiredString) {
    def asUuid() : RequiredUuidExtraction = RequiredUuidExtraction(requiredString)
  }

}

