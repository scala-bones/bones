package com.bones.interpreter.deltavalidator

import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.CustomStringValue

trait CustomStringDeltaValueInterpreter[IN]
    extends InterchangeFormatDeltaValidatorValue[CustomStringValue, IN] {

  val primitive: PrimitiveInterchangeFormat[IN, String]

  override def createDeltaValidator[A](
    alg: CustomStringValue[A]): DeltaValueValidator[String, CustomStringValue, A, IN] =
    primitive
      .extractString[CustomStringValue](alg.typeName)
      .asInstanceOf[DeltaValueValidator[String, CustomStringValue, A, IN]]

}
