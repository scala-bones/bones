package com.bones.sprayjson

import java.time.format.DateTimeFormatter

import com.bones.data.values._
import com.bones.interpreter.encoder.InterchangeFormatEncoderValue.CNilInterchangeFormatEncoder
import com.bones.interpreter.encoder.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  KvpInterchangeFormatEncoderInterpreter
}
import com.bones.interpreter.validator.InterchangeFormatValidatorValue.CNilInterchangeFormatValidator
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}
import com.bones.interpreter.values._
import com.bones.sprayjson.impl.{SprayPrimitiveEncoder, SprayPrimitiveValidator}
import shapeless.:+:
import spray.json.JsValue

package object values {
//  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, JsValue] =
//    BaseScalaCoreEncoder ++ (CustomStringEncoder ++ (BaseCirceIsoJavaTimeEncoder ++
//      (BaseJavaUtilEncoder ++ CNilInterchangeFormatEncoder[JsValue]())))
//
//  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, JsValue] =
//    BaseScalaCoreValidator ++ (CustomStringValidator ++ (BaseCirceIsoJavaTimeValidator ++
//      (BaseJavaUtilValidator ++ CNilInterchangeFormatValidator[JsValue]())))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, JsValue] = {
    InterchangeFormatEncoderValue.merge[ScalaCoreValue, CustomStringValueCo, JsValue](
      BaseScalaCoreEncoder,
      InterchangeFormatEncoderValue.merge[CustomStringValue, JavaTimeValueCo, JsValue](
        CustomStringEncoder,
        InterchangeFormatEncoderValue.merge[JavaTimeValue, JavaUtilValueCo, JsValue](
          BaseCirceIsoJavaTimeEncoder,
          InterchangeFormatEncoderValue
            .merge[JavaUtilValue, CNilF, JsValue](
              BaseJavaUtilEncoder,
              CNilInterchangeFormatEncoder[JsValue]())
        )
      )
    )
  }

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, JsValue] = {
    InterchangeFormatValidatorValue.merge[ScalaCoreValue, CustomStringValueCo, JsValue](
      BaseScalaCoreValidator,
      InterchangeFormatValidatorValue.merge[CustomStringValue, JavaTimeValueCo, JsValue](
        CustomStringValidator,
        InterchangeFormatValidatorValue.merge[JavaTimeValue, JavaUtilValueCo, JsValue](
          BaseCirceIsoJavaTimeValidator,
          InterchangeFormatValidatorValue
            .merge[JavaUtilValue, CNilF, JsValue](
              BaseJavaUtilValidator,
              CNilInterchangeFormatValidator[JsValue]())
        )
      )
    )
  }
  //end 2.12

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
