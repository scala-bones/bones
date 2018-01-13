package com.ot.bones.validation

trait MultiFieldValidation {

//  case class TowKeyExtraction[A,FGA<:FieldGroupOp[Validated[NonEmptyList[ExtractionError], A]],B,FGB<:FieldGroupOp[Validated[NonEmptyList[ExtractionError],B]]
//    (op1: FGA, op2: FGB, f: ValidationOp[(A,B)]) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], (A,B)]] {
//    override def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], (A,B)] = {
//      (op1.extract(producer), op2.extract(producer)).mapN()
//    }
//  }
//
//
//  val usPostalCodeRegex = "^\d{5}(-\d{4})?$".r
//
//  def zipCodeMatchesCountry(countryIso: RequiredString, zipCode: OptionalString): ValidationResultNel[Unit] = {
//    if (countryIso === "US") {
//      zipCode match {
//        case Some(zc) => usPostalCodeRegex.findFirstIn(zc)
//        case None => Left(MultiKeyError())
//      }
//    }
//  }


}
