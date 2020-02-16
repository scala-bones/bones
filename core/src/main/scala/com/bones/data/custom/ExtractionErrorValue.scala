package com.bones.data.custom

import com.bones.data.Error._
import com.bones.data.KvpSingleValueLeft
import com.bones.syntax.kvpCoNilCov

sealed abstract class ExtractionErrorValue[T]

case object CanNotConvertData extends ExtractionErrorValue[CanNotConvert[_, _]]
case object NotFoundData extends ExtractionErrorValue[NotFound[_]]
case object ParsingErrorData extends ExtractionErrorValue[ParsingError]
case object RequiredValueData extends ExtractionErrorValue[RequiredValue[_]]
case object SumTypeErrorData extends ExtractionErrorValue[SumTypeError]
case object SystemErrorData extends ExtractionErrorValue[SystemError]
case object ValidationErrorData extends ExtractionErrorValue[ValidationError[_]]
case object WrongTypeErrorData extends ExtractionErrorValue[WrongTypeError[_]]

trait ExtractionErrorValueSugar {

  val canNotConvert = CanNotConvertData
  val notFound = NotFoundData
  val parsingError = ParsingErrorData
  val requiredValue = RequiredValueData
  val sumTypeError = SumTypeErrorData
  val validationError = ValidationErrorData
  val wrongTypeError = WrongTypeErrorData

  def extractionErrors =
    canNotConvert :+>: notFound :+>: parsingError :+>: requiredValue :+>: sumTypeError :+>:
      validationError :+>: wrongTypeError :+>: kvpCoNilCov[ExtractionErrorValue]

}
