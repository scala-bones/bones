package com.bones.interpreter.deltavalidator

import com.bones.data.values.CustomStringValue

trait CustomStringDeltaValidatorValueCanBeOmitted[IN]
    extends InterchangeFormatDeltaValidatorValue[CustomStringValue, IN] {
  override def createDeltaValidator[A](
    alg: CustomStringValue[A]): DeltaValueValidator[String, CustomStringValue, A, IN] = ???

}
