package com.bones.argonaut

import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.interpreter.custom.{JavaTimeEncoder, JavaTimeValidator}
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}

package object custom {

  /** Encoder/Validator which uses default ISO format. */
  object ArgonautIsoJavaTimeEncoder
      extends JavaTimeEncoder[Json]
      with JavaTimeValidator[Json] {

    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter

    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

}
