package com.bones.json4s

import java.time.format.DateTimeFormatter

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.interpreter.InterchangeFormatEncoderValue.CNilInterchangeFormatEncoder
import com.bones.interpreter.InterchangeFormatValidatorValue.CNilInterchangeFormatValidator
import com.bones.interpreter.encoder.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  KvpInterchangeFormatEncoderInterpreter
}
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue
}
import com.bones.interpreter.values.{
  CustomStringEncoder,
  CustomStringValidator,
  ExtractionErrorEncoder,
  JavaTimeEncoder,
  JavaTimeValidator,
  JavaUtilEncoder,
  JavaUtilValidator,
  ScalaCoreEncoder,
  ScalaCoreValidator
}
import com.bones.json4s.impl.{Json4sPrimitiveEncoder, Json4sPrimitiveValidator}
import org.json4s.JValue
import shapeless.:+:

package object values {
//  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, JValue] =
//    BaseScalaCoreEncoder ++ (CustomStringEncoder ++ (BaseJson4sIsoJavaTimeEncoder ++
//      (BaseJavaUtilEncoder ++ CNilInterchangeFormatEncoder[JValue]())))
//
//  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, JValue] =
//    BaseScalaCoreValidator ++ (CustomStringValidator ++ (BaseJson4sIsoJavaTimeValidator ++
//      (BaseJavaUtilValidator ++ CNilInterchangeFormatValidator[JValue]())))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, JValue] = {
    InterchangeFormatEncoderValue.merge[ScalaCoreValue, CustomStringValueCo, JValue](
      BaseScalaCoreEncoder,
      InterchangeFormatEncoderValue.merge[CustomStringValue, JavaTimeValueCo, JValue](
        CustomStringEncoder,
        InterchangeFormatEncoderValue.merge[JavaTimeValue, JavaUtilValueCo, JValue](
          BaseJson4sIsoJavaTimeEncoder,
          InterchangeFormatEncoderValue
            .merge[JavaUtilValue, CNilF, JValue](
              BaseJavaUtilEncoder,
              CNilInterchangeFormatEncoder[JValue]())
        )
      )
    )
  }

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, JValue] = {
    InterchangeFormatValidatorValue.merge[ScalaCoreValue, CustomStringValueCo, JValue](
      BaseScalaCoreValidator,
      InterchangeFormatValidatorValue.merge[CustomStringValue, JavaTimeValueCo, JValue](
        CustomStringValidator,
        InterchangeFormatValidatorValue.merge[JavaTimeValue, JavaUtilValueCo, JValue](
          BaseJson4sIsoJavaTimeValidator,
          InterchangeFormatValidatorValue
            .merge[JavaUtilValue, CNilF, JValue](
              BaseJavaUtilValidator,
              CNilInterchangeFormatValidator[JValue]())
        )
      )
    )
  }
  //end 2.12

  object BaseScalaCoreEncoder extends ScalaCoreEncoder[JValue] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[JValue] =
      Json4sPrimitiveEncoder
  }

  object BaseScalaCoreValidator extends ScalaCoreValidator[JValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[JValue] =
      Json4sPrimitiveValidator
  }

  object BaseJavaUtilEncoder extends JavaUtilEncoder[JValue] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[JValue] =
      Json4sPrimitiveEncoder
  }

  object BaseJavaUtilValidator extends JavaUtilValidator[JValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[JValue] =
      Json4sPrimitiveValidator
  }

  object BaseJson4sIsoJavaTimeEncoder extends BaseJson4sIsoJavaTimeEncoder

  object BaseJson4sIsoJavaTimeValidator extends BaseJson4sIsoJavaTimeValidator

  trait BaseJson4sIsoJavaTimeValidator extends JavaTimeValidator[JValue] {

    override val baseValidator: InterchangeFormatPrimitiveValidator[JValue] =
      Json4sPrimitiveValidator
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  trait BaseJson4sIsoJavaTimeEncoder extends JavaTimeEncoder[JValue] {

    override val baseEncoder: InterchangeFormatPrimitiveEncoder[JValue] =
      Json4sPrimitiveEncoder
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
    override val localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override val localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override val localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  object CustomStringValidator extends CustomStringValidator[JValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[JValue] =
      Json4sPrimitiveValidator
  }

  object CustomStringEncoder extends CustomStringEncoder[JValue] {
    override val baseEncoder: InterchangeFormatPrimitiveEncoder[JValue] = Json4sPrimitiveEncoder
  }

  object BaseExtractionErrorEncoder extends ExtractionErrorEncoder[JValue] {

    override def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, JValue] =
      IsoJson4sEncoderInterpreter[ScalaCoreValue](
        BaseScalaCoreEncoder
      )

    override val scalaCoreInterpreter: ScalaCoreEncoder[JValue] = BaseScalaCoreEncoder
  }

  val isoJson4sEncoderInterpreter =
    new IsoJson4sEncoderInterpreter[DefaultValues](
      defaultEncoders
    )

  val isoJson4sValidatorInterpreter =
    new IsoJson4sValidatorInterpreter[DefaultValues](
      defaultValidators
    )

}
