package com.bones.circe

import java.time.format.DateTimeFormatter

import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator, ExtractionErrorEncoder, JavaTimeEncoder, JavaTimeValidator}
import io.circe.Json

package object custom {

  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, Json] =
    BaseCirceIsoJavaTimeEncoder ++
      (CustomStringEncoder ++ CNilInterchangeFormatEncoder[Json](): InterchangeFormatEncoder[CustomStringCoproduct, Json])

  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, Json] =
    BaseCirceIsoJavaTimeValidator ++
      (CustomStringValidator ++ CNilInterchangeFormatValidator[Json](): InterchangeFormatValidator[CustomStringCoproduct, Json])


  object BaseCirceIsoJavaTimeEncoder extends BaseCirceIsoJavaTimeEncoder {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  object BaseCirceIsoJavaTimeValidator extends BaseCirceIsoJavaTimeValidator {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  trait BaseCirceIsoJavaTimeValidator
    extends JavaTimeValidator[Json] {

    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  }

  trait BaseCirceIsoJavaTimeEncoder extends JavaTimeEncoder[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

  object CustomStringValidator extends CustomStringValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  object CustomStringEncoder extends CustomStringEncoder[Json] {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  object BaseExtractionErrorEncoder extends ExtractionErrorEncoder[Json] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

}
