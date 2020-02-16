package com.bones.circe

import java.time.format.DateTimeFormatter

import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{JavaTimeEncoder, JavaTimeValidator}
import io.circe.Json

package object custom {
  /** Encoder/Validator which uses default ISO format. */
  object CirceIsoJavaTimeEncoder
    extends JavaTimeEncoder[Json]
      with JavaTimeValidator[Json] {

    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter

    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }
}
