package com.bones.argonaut

import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator, ExtractionErrorEncoder, JavaTimeEncoder, JavaTimeValidator}
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}

package object custom {


  // Encoder for the coproduct of all custom algebras
  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, Json] =
    ArgonautIsoJavaTimeEncoder ++
      (CustomStringEncoder ++ CNilInterchangeFormatEncoder[Json](): InterchangeFormatEncoder[CustomStringCoproduct, Json])

  // Validator for the coproduct of all custom algebras
  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, Json] =
    ArgonautIsoJavaTimeValidator ++
      (CustomStringValidator ++ CNilInterchangeFormatValidator[Json](): InterchangeFormatValidator[CustomStringCoproduct, Json])


  object ArgonautIsoJavaTimeValidator extends BaseArgonautIsoJavaTimeValidator {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautIsoJavaTimeEncoder extends BaseArgonautIsoJavaTimeEncoder {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  trait BaseArgonautIsoJavaTimeValidator extends JavaTimeValidator[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  }

  /** Encoder/Validator which uses default ISO format. */
  trait BaseArgonautIsoJavaTimeEncoder
    extends JavaTimeEncoder[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

  object CustomStringValidator extends CustomStringValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object CustomStringEncoder extends CustomStringEncoder[Json] {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object BaseExtractionErrorEncoder extends ExtractionErrorEncoder[Json] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }



}
