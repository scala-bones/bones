package com.bones.data.custom

import com.bones.data.Error._
import com.bones.data.{KvpSingleValueLeft, Sugar}
import shapeless.{:+:, CNil}

sealed abstract class ExtractionErrorValue[T]

case object CanNotConvertData extends ExtractionErrorValue[CanNotConvert[_, _]]
case object NotFoundData extends ExtractionErrorValue[NotFound[_]]
case object ParsingErrorData extends ExtractionErrorValue[ParsingError]
case object RequiredValueData extends ExtractionErrorValue[RequiredValue[_]]
case object SumTypeErrorData extends ExtractionErrorValue[SumTypeError]
case object SystemErrorData extends ExtractionErrorValue[SystemError]
case object ValidationErrorData extends ExtractionErrorValue[ValidationError[_]]
case object WrongTypeErrorData extends ExtractionErrorValue[WrongTypeError[_]]

/**
  * Provides convenience methods for creating ExtractionErrorValue types.
  */
trait ExtractionErrorValueSugar extends Sugar[ExtractionErrorValue] {

  val canNotConvert: ExtractionErrorValue[CanNotConvert[_, _]] = CanNotConvertData
  val notFound: ExtractionErrorValue[NotFound[_]] = NotFoundData
  val parsingError: ExtractionErrorValue[ParsingError] = ParsingErrorData
  val requiredValue: ExtractionErrorValue[RequiredValue[_]] = RequiredValueData
  val sumTypeError: ExtractionErrorValue[SumTypeError] = SumTypeErrorData
  val validationError: ExtractionErrorValue[ValidationError[_]] = ValidationErrorData
  val wrongTypeError: ExtractionErrorValue[WrongTypeError[_]] = WrongTypeErrorData

  def extractionErrors: KvpSingleValueLeft[ExtractionErrorValue,CanNotConvert[_, _],
    NotFound[_] :+: ParsingError :+: RequiredValue[_] :+: SumTypeError :+: ValidationError[_] :+:
      WrongTypeError[_] :+: CNil] =
    canNotConvert :+>: notFound :+>: parsingError :+>: requiredValue :+>: sumTypeError :+>:
      validationError :+>: wrongTypeError :+>: kvpCoNil

}
