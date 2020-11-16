package com.bones.sprayjson

import java.time.format.DateTimeFormatter

import com.bones.data.values.{DefaultValues, ScalaCoreValue}
import com.bones.interpreter.InterchangeFormatEncoderValue.CNilInterchangeFormatEncoder
import com.bones.interpreter.InterchangeFormatValidatorValue.CNilInterchangeFormatValidator
import com.bones.interpreter._
import com.bones.interpreter.values._
import com.bones.sprayjson.impl.{SprayPrimitiveEncoder, SprayPrimitiveValidator}
import spray.json.JsValue

package object values {
  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, JsValue] =
    BaseScalaCoreEncoder ++ (CustomStringEncoder ++ (BaseCirceIsoJavaTimeEncoder ++
      (BaseJavaUtilEncoder ++ CNilInterchangeFormatEncoder[JsValue]())))

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, JsValue] =
    BaseScalaCoreValidator ++ (CustomStringValidator ++ (BaseCirceIsoJavaTimeValidator ++
      (BaseJavaUtilValidator ++ CNilInterchangeFormatValidator[JsValue]())))

  object BaseScalaCoreEncoder extends ScalaCoreEncoder[JsValue] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[JsValue] =
      SprayPrimitiveEncoder
  }

  object BaseScalaCoreValidator extends ScalaCoreValidator[JsValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[JsValue] =
      SprayPrimitiveValidator
  }

  object BaseJavaUtilEncoder extends JavaUtilEncoder[JsValue] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[JsValue] =
      SprayPrimitiveEncoder
  }

  object BaseJavaUtilValidator extends JavaUtilValidator[JsValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[JsValue] =
      SprayPrimitiveValidator
  }

  object BaseCirceIsoJavaTimeEncoder extends BaseCirceIsoJavaTimeEncoder

  object BaseCirceIsoJavaTimeValidator extends BaseCirceIsoJavaTimeValidator

  trait BaseCirceIsoJavaTimeValidator extends JavaTimeValidator[JsValue] {

    override val baseValidator: InterchangeFormatPrimitiveValidator[JsValue] =
      SprayPrimitiveValidator
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  trait BaseCirceIsoJavaTimeEncoder extends JavaTimeEncoder[JsValue] {

    override val baseEncoder: InterchangeFormatPrimitiveEncoder[JsValue] =
      SprayPrimitiveEncoder
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  object CustomStringValidator extends CustomStringValidator[JsValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[JsValue] =
      SprayPrimitiveValidator
  }

  object CustomStringEncoder extends CustomStringEncoder[JsValue] {
    override val baseEncoder: InterchangeFormatPrimitiveEncoder[JsValue] = SprayPrimitiveEncoder
  }

  object BaseExtractionErrorEncoder extends ExtractionErrorEncoder[JsValue] {

    override def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, JsValue] =
      IsoSprayEncoderInterpreter[ScalaCoreValue](
        BaseScalaCoreEncoder
      )

    override val scalaCoreInterpreter: ScalaCoreEncoder[JsValue] = BaseScalaCoreEncoder
  }

  val isoSprayEncoderInterpreter =
    new IsoSprayEncoderInterpreter[DefaultValues](
      defaultEncoders
    )

  val isoSprayValidatorInterpreter =
    new IsoSprayValidatorInterpreter[DefaultValues](
      defaultValidators
    )

}
