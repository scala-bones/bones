package com.bones.argonaut

import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.data.values.DefaultValues
import com.bones.interpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.values._
import com.bones.interpreter.{InterchangeFormatEncoder, InterchangeFormatValidator, KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}

package object values {

  val defaultEncoders: InterchangeFormatEncoder[DefaultValues, Json] =
    ArgonautScalaCoreEncoder ++
      (CustomStringEncoder ++
        (ArgonautIsoJavaTimeEncoder ++
          (ArgonautJavaUtilEncoder ++ CNilInterchangeFormatEncoder[Json]())))

  // Validator for the coproduct of all custom algebras
  val defaultValidators: InterchangeFormatValidator[DefaultValues, Json] =
    ArgonautScalaCoreValidator ++
      (CustomStringValidator ++
        (ArgonautIsoJavaTimeValidator ++
          (ArgonautJavaUtilValidator ++ CNilInterchangeFormatValidator[Json]())))

  object ArgonautScalaCoreValidator extends ScalaCoreValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautScalaCoreEncoder extends ScalaCoreEncoder[Json] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautIsoJavaTimeValidator extends BaseArgonautIsoJavaTimeValidator {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautIsoJavaTimeEncoder extends BaseArgonautIsoJavaTimeEncoder {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautJavaUtilEncoder extends JavaUtilEncoder[Json] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautJavaUtilValidator extends JavaUtilValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  trait BaseArgonautIsoJavaTimeValidator extends JavaTimeValidator[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME

  }

  /** Encoder/Validator which uses default ISO format. */
  trait BaseArgonautIsoJavaTimeEncoder extends JavaTimeEncoder[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME

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
    override def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter

    override val scalaCoreInterpreter: ScalaCoreEncoder[Json] = ArgonautScalaCoreEncoder
  }

}
