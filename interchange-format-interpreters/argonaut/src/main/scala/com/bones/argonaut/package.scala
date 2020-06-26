package com.bones

package object argonaut {

  /** An implementation of an Argonaut Encoder and and Validator using Standard ISO DATE string
    * format for serializing.
    * Implement both Encoder and Validator to ensure consistent data formats and coproductTypeKey */
  object IsoArgonautEncoderAndValidatorInterpreter
      extends ArgonautEncoderInterpreter
      with ArgonautValidatorInterpreter {

    override val coproductTypeKey: String = "type"
  }

}
