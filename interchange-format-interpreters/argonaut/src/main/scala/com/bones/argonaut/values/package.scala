package com.bones.argonaut

import java.time.format.DateTimeFormatter

import argonaut.Json
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
import com.bones.interpreter.values._
import com.bones.interpreter._
import shapeless.:+:

package object values {

//  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, Json] =
//    ArgonautScalaCoreEncoder ++
//      (CustomStringEncoder ++
//        (ArgonautIsoJavaTimeEncoder ++
//          (ArgonautJavaUtilEncoder ++ CNilInterchangeFormatEncoder[Json]())))
//
//  // Validator for the coproduct of all custom algebras
//  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, Json] =
//    ArgonautScalaCoreValidator ++
//      (CustomStringValidator ++
//        (ArgonautIsoJavaTimeValidator ++
//          (ArgonautJavaUtilValidator ++ CNilInterchangeFormatValidator[Json]())))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, Json] = {
    InterchangeFormatEncoderValue.merge[ScalaCoreValue, CustomStringValueCo, Json](
      ArgonautScalaCoreEncoder,
      InterchangeFormatEncoderValue.merge[CustomStringValue, JavaTimeValueCo, Json](
        CustomStringEncoder,
        InterchangeFormatEncoderValue.merge[JavaTimeValue, JavaUtilValueCo, Json](
          ArgonautIsoJavaTimeEncoder,
          InterchangeFormatEncoderValue
            .merge[JavaUtilValue, CNilF, Json](
              ArgonautJavaUtilEncoder,
              CNilInterchangeFormatEncoder[Json]())
        )
      )
    )
  }

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, Json] = {
    InterchangeFormatValidatorValue.merge[ScalaCoreValue, CustomStringValueCo, Json](
      ArgonautScalaCoreValidator,
      InterchangeFormatValidatorValue.merge[CustomStringValue, JavaTimeValueCo, Json](
        CustomStringValidator,
        InterchangeFormatValidatorValue.merge[JavaTimeValue, JavaUtilValueCo, Json](
          ArgonautIsoJavaTimeValidator,
          InterchangeFormatValidatorValue
            .merge[JavaUtilValue, CNilF, Json](
              ArgonautJavaUtilValidator,
              CNilInterchangeFormatValidator[Json]())
        )
      )
    )
  }
  //end 2.12

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
