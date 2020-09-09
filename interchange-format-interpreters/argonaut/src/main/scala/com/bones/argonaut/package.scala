package com.bones

import _root_.argonaut.Json
import com.bones.data.values.DefaultValues
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}

package object argonaut {

  /** An implementation of an Argonaut Encoder and and Validator using Standard ISO DATE string
    * format for serializing.
    * Implement both Encoder and Validator to ensure consistent data formats and coproductTypeKey */
  case class IsoArgonautValidatorInterpreter(
    override val interchangeFormatValidator: InterchangeFormatValidatorValue[DefaultValues, Json]
  ) extends ArgonautValidatorInterpreter[DefaultValues] {

    override val coproductTypeKey: String = "type"
    override val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[Json] =
      ArgonautPrimitiveValidator
  }

  case class IsoArgonautEncoderInterpreter(
    override val encoder: InterchangeFormatEncoderValue[DefaultValues, Json]
  ) extends ArgonautEncoderInterpreter[DefaultValues] {
    override def coproductTypeKey: String = "type"

    override def interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      ArgonautPrimitiveEncoder
  }

  val defaultIsoArgonautValidatorInterpreter: IsoArgonautValidatorInterpreter =
    IsoArgonautValidatorInterpreter(values.defaultValidators)
  val defaultIsoArgonautEncoderInterpreter: IsoArgonautEncoderInterpreter =
    IsoArgonautEncoderInterpreter(values.defaultEncoders)

}
