package com.bones

import com.bones.data.values.DefaultValues
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}
import io.circe.Json

package object circe {

  case class IsoCirceEncoderAndValidatorInterpreter[ALG[_]](
    override val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, Json],
    override val encoder: InterchangeFormatEncoderValue[ALG, Json])
      extends CirceEncoderInterpreter[ALG]
      with CirceValidatorInterpreter[ALG] {

    override val coproductTypeKey: String = "type"

    override val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[Json] =
      CircePrimitiveValidator
    override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      CircePrimitiveEncoder
  }

}
