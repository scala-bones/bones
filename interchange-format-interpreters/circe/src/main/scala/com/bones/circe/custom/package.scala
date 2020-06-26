package com.bones.circe

import java.time.format.DateTimeFormatter

import com.bones.data.custom.AllCustomAlgebras
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator, ExtractionErrorEncoder, JavaTimeEncoder, JavaTimeValidator, JavaUtilEncoder, JavaUtilValidator, ScalaCoreEncoder, ScalaCoreValidator}
import io.circe.Json

package object custom {

  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, Json] =
    BaseScalaCoreEncoder ++  (CustomStringEncoder ++ (BaseCirceIsoJavaTimeEncoder ++
      (BaseJavaUtilEncoder ++ CNilInterchangeFormatEncoder[Json]())))

  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, Json] =
    BaseScalaCoreValidator ++ (CustomStringValidator ++ (BaseCirceIsoJavaTimeValidator ++
      (BaseJavaUtilValidator ++ CNilInterchangeFormatValidator[Json]())))

  object BaseScalaCoreEncoder extends ScalaCoreEncoder[Json] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  object BaseScalaCoreValidator extends ScalaCoreValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  object BaseJavaUtilEncoder extends JavaUtilEncoder[Json] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  object BaseJavaUtilValidator extends JavaUtilValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

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
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

  trait BaseCirceIsoJavaTimeEncoder extends JavaTimeEncoder[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
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
    override val scalaCoreInterpreter: ScalaCoreEncoder[Json] = ScalaCoreEncoder

  }

}
