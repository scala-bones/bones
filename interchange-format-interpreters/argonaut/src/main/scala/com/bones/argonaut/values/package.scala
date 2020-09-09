package com.bones.argonaut

import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.data.values.{DefaultValues, ScalaCoreValue}
import com.bones.interpreter.InterchangeFormatEncoderValue.CNilInterchangeFormatEncoder
import com.bones.interpreter.InterchangeFormatValidatorValue.CNilInterchangeFormatValidator
import com.bones.interpreter.values._
import com.bones.interpreter._

package object values {

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, Json] =
    ArgonautScalaCoreEncoder ++
      (CustomStringEncoder ++
        (ArgonautIsoJavaTimeEncoder ++
          (ArgonautJavaUtilEncoder ++ CNilInterchangeFormatEncoder[Json]())))

  // Validator for the coproduct of all custom algebras
  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, Json] =
    ArgonautScalaCoreValidator ++
      (CustomStringValidator ++
        (ArgonautIsoJavaTimeValidator ++
          (ArgonautJavaUtilValidator ++ CNilInterchangeFormatValidator[Json]())))

  object ArgonautScalaCoreValidator extends ScalaCoreValidator[Json] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      ArgonautPrimitiveValidator
  }

  object ArgonautScalaCoreEncoder extends ScalaCoreEncoder[Json] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      ArgonautPrimitiveEncoder
  }

  object ArgonautIsoJavaTimeValidator extends BaseArgonautIsoJavaTimeValidator {
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      ArgonautPrimitiveValidator
  }

  object ArgonautIsoJavaTimeEncoder extends BaseArgonautIsoJavaTimeEncoder {
    override val baseEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      ArgonautPrimitiveEncoder
  }

  object ArgonautJavaUtilEncoder extends JavaUtilEncoder[Json] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      ArgonautPrimitiveEncoder
  }

  object ArgonautJavaUtilValidator extends JavaUtilValidator[Json] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      ArgonautPrimitiveValidator
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
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      ArgonautPrimitiveValidator
  }

  object CustomStringEncoder extends CustomStringEncoder[Json] {
    override val baseEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      ArgonautPrimitiveEncoder
  }

  object IsoArgonautScalaCoreEncoder extends ArgonautEncoderInterpreter[ScalaCoreValue] {
    override def coproductTypeKey: String = "type"

    override def encoder: InterchangeFormatEncoderValue[ScalaCoreValue, Json] =
      ArgonautScalaCoreEncoder

    override def interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      ArgonautPrimitiveEncoder
  }

  object BaseExtractionErrorEncoder extends ExtractionErrorEncoder[Json] {

    override val scalaCoreInterpreter: ScalaCoreEncoder[Json] =
      ArgonautScalaCoreEncoder

    override def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, Json] =
      IsoArgonautScalaCoreEncoder

  }

}
