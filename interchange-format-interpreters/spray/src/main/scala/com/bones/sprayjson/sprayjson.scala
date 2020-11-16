package com.bones

import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}
import com.bones.sprayjson.impl.{SprayPrimitiveEncoder, SprayPrimitiveValidator}
import spray.json.JsValue

package object sprayjson {

  val coproductTypeKeyConst: String = "type"

  case class IsoSprayEncoderInterpreter[ALG[_]](
    override val encoder: InterchangeFormatEncoderValue[ALG, JsValue]
  ) extends SprayEncoderInterpreter[ALG] {
    override val coproductTypeKey: String = coproductTypeKeyConst
    override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[JsValue] =
      SprayPrimitiveEncoder
  }

  case class IsoSprayValidatorInterpreter[ALG[_]](
    override val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, JsValue]
  ) extends SprayValidatorInterpreter[ALG] {

    override val coproductTypeKey: String = coproductTypeKeyConst
    override val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[JsValue] =
      SprayPrimitiveValidator
  }

}
