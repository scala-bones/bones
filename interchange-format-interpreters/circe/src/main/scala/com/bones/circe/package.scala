package com.bones

import com.bones.interpreter.encoder.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder
}
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}
import io.circe.Json

package object circe {

  val coproductTypeKey: String = "type"

  case class IsoCirceEncoderInterpreter[ALG[_]](
    override val encoder: InterchangeFormatEncoderValue[ALG, Json]
  ) extends CirceEncoderInterpreter[ALG] {
    override val coproductTypeKey: String = circe.coproductTypeKey
    override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      CircePrimitiveEncoder
  }

  case class IsoCirceValidatorInterpreter[ALG[_]](
    override val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, Json]
  ) extends CirceValidatorInterpreter[ALG] {

    override val coproductTypeKey: String = circe.coproductTypeKey
    override val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[Json] =
      CircePrimitiveValidator
  }

}
