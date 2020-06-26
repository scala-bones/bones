package com.bones

package object circe {

  object IsoCirceEncoderAndValidatorInterpreter
      extends CirceEncoderInterpreter
      with CirceValidatorInterpreter {

    override val coproductTypeKey: String = "type"

  }

}
