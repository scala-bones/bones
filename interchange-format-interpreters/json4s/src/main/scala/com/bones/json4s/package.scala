package com.bones

import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}
import com.bones.json4s.impl.{Json4sPrimitiveEncoder, Json4sPrimitiveValidator}
import org.json4s.JValue

package object json4s {

  val coproductTypeKeyConst: String = "type"

  case class IsoJson4sEncoderInterpreter[ALG[_]](
    override val encoder: InterchangeFormatEncoderValue[ALG, JValue]
  ) extends Json4sEncoderInterpreter[ALG] {
    override val coproductTypeKey: String = coproductTypeKeyConst
    override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[JValue] =
      Json4sPrimitiveEncoder
  }

  case class IsoJson4sValidatorInterpreter[ALG[_]](
    override val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, JValue]
  ) extends Json4sValidatorInterpreter[ALG] {

    override val coproductTypeKey: String = coproductTypeKeyConst
    override val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[JValue] =
      Json4sPrimitiveValidator
  }

}
