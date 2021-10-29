package com.bones.circe

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
import io.circe.Json
import shapeless.:+:

package object values {

//  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, Json] =
//    BaseScalaCoreEncoder ++ (CustomStringEncoder ++ (BaseCirceIsoJavaTimeEncoder ++
//      (BaseJavaUtilEncoder ++ CNilInterchangeFormatEncoder[Json]())))
//
//  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, Json] =
//    BaseScalaCoreValidator ++ (CustomStringValidator ++ (BaseCirceIsoJavaTimeValidator ++
//      (BaseJavaUtilValidator ++ CNilInterchangeFormatValidator[Json]())))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, Json] = {
    InterchangeFormatEncoderValue.merge[ScalaCoreValue, CustomStringValueCo, Json](
      BaseScalaCoreEncoder,
      InterchangeFormatEncoderValue.merge[CustomStringValue, JavaTimeValueCo, Json](
        CustomStringEncoder,
        InterchangeFormatEncoderValue.merge[JavaTimeValue, JavaUtilValueCo, Json](
          BaseCirceIsoJavaTimeEncoder,
          InterchangeFormatEncoderValue
            .merge[JavaUtilValue, CNilF, Json](
              BaseJavaUtilEncoder,
              CNilInterchangeFormatEncoder[Json]()
            )
        )
      )
    )
  }

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, Json] = {
    InterchangeFormatValidatorValue.merge[ScalaCoreValue, CustomStringValueCo, Json](
      BaseScalaCoreValidator,
      InterchangeFormatValidatorValue.merge[CustomStringValue, JavaTimeValueCo, Json](
        CustomStringValidator,
        InterchangeFormatValidatorValue.merge[JavaTimeValue, JavaUtilValueCo, Json](
          BaseCirceIsoJavaTimeValidator,
          InterchangeFormatValidatorValue
            .merge[JavaUtilValue, CNilF, Json](
              BaseJavaUtilValidator,
              CNilInterchangeFormatValidator[Json]()
            )
        )
      )
    )
  }
  // end 2.12

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
