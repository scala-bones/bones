package com.bones.bson

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
import reactivemongo.bson.BSONValue
import shapeless.:+:

package object values {

//  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, BSONValue] =
//    BsonScalaCoreEncoder ++
//      (DefaultBsonCustomStringEncoder ++
//        (IsoBsonJavaTimeEncoder ++
//          (BsonJavaUtilEncoder ++ CNilInterchangeFormatEncoder[BSONValue]())))
//
//  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, BSONValue] =
//    BsonScalaCoreValidator ++
//      (DefaultBsonCustomStringValidator ++
//        (IsoBsonJavaTimeValidator ++
//          (BsonJavaUtilValidator ++ CNilInterchangeFormatValidator[BSONValue]())))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  //start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: InterchangeFormatEncoderValue[DefaultValues, BSONValue] = {
    InterchangeFormatEncoderValue.merge[ScalaCoreValue, CustomStringValueCo, BSONValue](
      BsonScalaCoreEncoder,
      InterchangeFormatEncoderValue.merge[CustomStringValue, JavaTimeValueCo, BSONValue](
        DefaultBsonCustomStringEncoder,
        InterchangeFormatEncoderValue.merge[JavaTimeValue, JavaUtilValueCo, BSONValue](
          IsoBsonJavaTimeEncoder,
          InterchangeFormatEncoderValue
            .merge[JavaUtilValue, CNilF, BSONValue](
              BsonJavaUtilEncoder,
              CNilInterchangeFormatEncoder[BSONValue]()
            )
        )
      )
    )
  }

  val defaultValidators: InterchangeFormatValidatorValue[DefaultValues, BSONValue] = {
    InterchangeFormatValidatorValue.merge[ScalaCoreValue, CustomStringValueCo, BSONValue](
      BsonScalaCoreValidator,
      InterchangeFormatValidatorValue.merge[CustomStringValue, JavaTimeValueCo, BSONValue](
        DefaultBsonCustomStringValidator,
        InterchangeFormatValidatorValue.merge[JavaTimeValue, JavaUtilValueCo, BSONValue](
          IsoBsonJavaTimeValidator,
          InterchangeFormatValidatorValue
            .merge[JavaUtilValue, CNilF, BSONValue](
              BsonJavaUtilValidator,
              CNilInterchangeFormatValidator[BSONValue]()
            )
        )
      )
    )
  }
  //end 2.12

  /** Uses the default encoder interpreter for BSON */
  object DefaultBsonCustomStringEncoder extends CustomStringEncoder[BSONValue] {
    override val baseEncoder: InterchangeFormatPrimitiveEncoder[BSONValue] =
      BsonPrimitiveEncoder
  }

  /** Uses the default validator interpreter for BSON */
  object DefaultBsonCustomStringValidator extends CustomStringValidator[BSONValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[BSONValue] =
      BsonPrimitiveValidator
  }

  object BsonScalaCoreEncoder extends ScalaCoreEncoder[BSONValue] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[BSONValue] =
      BsonPrimitiveEncoder
  }

  object BsonScalaCoreValidator extends ScalaCoreValidator[BSONValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[BSONValue] =
      BsonPrimitiveValidator
  }

  object BsonJavaUtilEncoder extends JavaUtilEncoder[BSONValue] {
    override val defaultEncoder: InterchangeFormatPrimitiveEncoder[BSONValue] =
      BsonPrimitiveEncoder
  }

  object BsonJavaUtilValidator extends JavaUtilValidator[BSONValue] {
    override val baseValidator: InterchangeFormatPrimitiveValidator[BSONValue] =
      BsonPrimitiveValidator
  }

  /** Encoder/Validator which uses default ISO format. */
  object IsoBsonJavaTimeEncoder extends BsonJavaTimeEncoder {
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

  object IsoBsonJavaTimeValidator extends BsonJavaTimeValidator {
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

  object BsonScalaCoreValueEncoderInterpreter extends BsonEncoderInterpreter[ScalaCoreValue] {
    override def encoder: InterchangeFormatEncoderValue[ScalaCoreValue, BSONValue] =
      BsonScalaCoreEncoder

    override def interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[BSONValue] =
      BsonPrimitiveEncoder
  }

  object DefaultBsonErrorEncoder extends ExtractionErrorEncoder[BSONValue] {

    override def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, BSONValue] =
      BsonScalaCoreValueEncoderInterpreter

    override val scalaCoreInterpreter: ScalaCoreEncoder[BSONValue] =
      BsonScalaCoreEncoder
  }

  val defaultBsonValidatorInterpreter = new BsonValidatorInterpreter[DefaultValues] {
    override val interchangeFormatValidator
      : InterchangeFormatValidatorValue[DefaultValues, BSONValue] =
      defaultValidators

    override val interchangeFormatPrimitiveValidator
      : InterchangeFormatPrimitiveValidator[BSONValue] =
      BsonPrimitiveValidator
  }

  val defaultBsonEncoderInterpreter = new BsonEncoderInterpreter[DefaultValues] {
    override def encoder: InterchangeFormatEncoderValue[DefaultValues, BSONValue] =
      defaultEncoders

    override def interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[BSONValue] =
      BsonPrimitiveEncoder
  }

}
