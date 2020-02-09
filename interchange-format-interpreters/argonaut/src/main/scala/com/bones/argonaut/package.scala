package com.bones

import java.time.format.DateTimeFormatter

import _root_.argonaut.Json
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.DateToStringEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.StringToDateValidator

package object argonaut {

  /** An implementation of an Argonaut Encoder and and Validator using Standard ISO DATE string
    * format for serializing.
    * Implement both Encoder and Validator to ensure consistent data formats and coproductTypeKey */
  object IsoArgonautEncoderAndValidatorInterpreter
      extends ArgonautEncoderInterpreter
      with ArgonautValidatorInterpreter
      with DateToStringEncoder[Json]
      with StringToDateValidator[Json] {

    override val coproductTypeKey: String = "type"
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override def localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override def localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

}
