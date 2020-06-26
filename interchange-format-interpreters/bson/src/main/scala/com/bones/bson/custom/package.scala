package com.bones.bson

import java.time.format.DateTimeFormatter

import com.bones.data.custom.AllCustomAlgebras
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.custom._
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import reactivemongo.bson.BSONValue

package object custom {

  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, BSONValue] =
    BsonScalaCoreEncoder ++
      (DefaultBsonCustomStringEncoder ++
          (IsoBsonJavaTimeEncoder ++
              (BsonJavaUtilEncoder ++ CNilInterchangeFormatEncoder[BSONValue]())))

  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, BSONValue] =
    BsonScalaCoreValidator ++
      (DefaultBsonCustomStringValidator ++
        (IsoBsonJavaTimeValidator ++
          (BsonJavaUtilValidator ++ CNilInterchangeFormatValidator[BSONValue]())))

  /** Uses the default encoder interpreter for BSON */
  object DefaultBsonCustomStringEncoder extends CustomStringEncoder[BSONValue] {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] =
      BsonEncoderInterpreter
  }

  /** Uses the default validator interpreter for BSON */
  object DefaultBsonCustomStringValidator extends CustomStringValidator[BSONValue] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[BSONValue] =
      BsonValidatorInterpreter
  }

  object BsonScalaCoreEncoder extends ScalaCoreEncoder[BSONValue] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] =
      BsonEncoderInterpreter
  }

  object BsonScalaCoreValidator extends ScalaCoreValidator[BSONValue] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[BSONValue] =
      BsonValidatorInterpreter
  }

  object BsonJavaUtilEncoder extends JavaUtilEncoder[BSONValue] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] =
      BsonEncoderInterpreter
  }

  object BsonJavaUtilValidator extends JavaUtilValidator[BSONValue] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[BSONValue] =
      BsonValidatorInterpreter
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

  object DefaultBsonErrorEncoder extends ExtractionErrorEncoder[BSONValue] {
    override val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] =
      BsonEncoderInterpreter
    override val scalaCoreInterpreter: ScalaCoreEncoder[BSONValue] =
      BsonScalaCoreEncoder
  }

}
