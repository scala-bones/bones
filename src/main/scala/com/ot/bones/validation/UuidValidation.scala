package com.ot.bones.validation

import java.util.UUID

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.ot.bones.{BonesOp, Key}
import com.ot.bones.compiler.ExtractionCompiler.{ExtractionError, ExtractionErrors, ExtractionOp, JsonProducer, ValidationError, ValidationResultNel}
import com.ot.bones.validation.StringValidation.{OptionalString, RequiredString}
import com.ot.bones.validation.UuidValidation.{OptionalUuidExtraction, RequiredUuidExtraction}

object UuidValidation {

  object IsUuid extends ExtractionOp[UUID] {
    override def description: String = "Is a UUID"
  }


  def convert(uuidString: String, key: => Key): Validated[ExtractionErrors, UUID] = try {
    Valid(UUID.fromString(uuidString))
  } catch {
    case _: IllegalArgumentException => Invalid(NonEmptyList.one(ValidationError(key, IsUuid, Some(uuidString))))
  }


  case class RequiredUuidExtraction(stringExtraction: RequiredString) extends BonesOp[UUID] {

    def extract(jsonProducer: JsonProducer): ValidationResultNel[UUID] =
      stringExtraction.extract(jsonProducer).andThen(convert(_,stringExtraction.key))
  }

  case class OptionalUuidExtraction(stringExtraction: OptionalString) extends BonesOp[Option[UUID]] {
    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Option[UUID]] =
      stringExtraction.extract(producer).andThen {
        case Some(uuidStr) => convert(uuidStr, stringExtraction.key).map(Some(_))
        case None => Valid(None)
      }
  }
}

trait UuidValidation {
  import StringValidation._

  implicit class OptionalStringToUuid(optionalString: OptionalString) {
    def asUuid() : OptionalUuidExtraction = OptionalUuidExtraction(optionalString)
  }

  implicit class RequiredStringToUuid(requiredString: RequiredString) {
    def asUuid() : RequiredUuidExtraction = RequiredUuidExtraction(requiredString)
  }

}

