package com.bones.circe

import java.time.format.DateTimeFormatter

import com.bones.data.values.{DefaultValues, ScalaCoreValue}
import com.bones.interpreter.InterchangeFormatEncoderValue.CNilInterchangeFormatEncoder
import com.bones.interpreter.InterchangeFormatValidatorValue.CNilInterchangeFormatValidator
import com.bones.interpreter.values._
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  KvpInterchangeFormatEncoderInterpreter
}
import io.circe.Json

package object values {

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, Json] =
    BaseScalaCoreEncoder ++ (CustomStringEncoder ++ (BaseCirceIsoJavaTimeEncoder ++
      (BaseJavaUtilEncoder ++ CNilInterchangeFormatEncoder[Json]())))

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, Json] =
    BaseScalaCoreValidator ++ (CustomStringValidator ++ (BaseCirceIsoJavaTimeValidator ++
      (BaseJavaUtilValidator ++ CNilInterchangeFormatValidator[Json]())))

  object BaseScalaCoreEncoder extends ScalaCoreEncoder[Json] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      CircePrimitiveEncoder
  }

  object BaseScalaCoreValidator extends ScalaCoreValidator[Json] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      CircePrimitiveValidator
  }

  object BaseJavaUtilEncoder extends JavaUtilEncoder[Json] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      CircePrimitiveEncoder
  }

  object BaseJavaUtilValidator extends JavaUtilValidator[Json] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      CircePrimitiveValidator
  }

  object BaseCirceIsoJavaTimeEncoder extends BaseCirceIsoJavaTimeEncoder

  object BaseCirceIsoJavaTimeValidator extends BaseCirceIsoJavaTimeValidator

  trait BaseCirceIsoJavaTimeValidator extends JavaTimeValidator[Json] {

    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      CircePrimitiveValidator
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  trait BaseCirceIsoJavaTimeEncoder extends JavaTimeEncoder[Json] {

    override val baseEncoder: InterchangeFormatPrimitiveEncoder[Json] =
      CircePrimitiveEncoder
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  object CustomStringValidator extends CustomStringValidator[Json] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[Json] =
      CircePrimitiveValidator
  }

  object CustomStringEncoder extends CustomStringEncoder[Json] {
    override val baseEncoder: InterchangeFormatPrimitiveEncoder[Json] = CircePrimitiveEncoder
  }

  object BaseExtractionErrorEncoder extends ExtractionErrorEncoder[Json] {

    override def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, Json] =
      IsoCirceEncoderInterpreter[ScalaCoreValue](
        BaseScalaCoreEncoder
      )

    override val scalaCoreInterpreter: ScalaCoreEncoder[Json] = BaseScalaCoreEncoder
  }

  val isoCirceEncoderInterpreter =
    new IsoCirceEncoderInterpreter[DefaultValues](
      values.defaultEncoders
    )

  val isoCirceValidatorInterpreter =
    new IsoCirceValidatorInterpreter[DefaultValues](
      values.defaultValidators
    )

}
