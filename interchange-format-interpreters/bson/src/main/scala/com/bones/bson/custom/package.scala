package com.bones.bson

import java.time.format.DateTimeFormatter

import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator, ExtractionErrorEncoder}
import reactivemongo.bson.BSONValue

package object custom {


  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, BSONValue] =
    IsoBsonJavaTimeEncoder ++
      (DefaultBsonCustomStringEncoder ++ CNilInterchangeFormatEncoder[BSONValue](): InterchangeFormatEncoder[CustomStringCoproduct, BSONValue])

  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, BSONValue] =
    IsoBsonJavaTimeValidator ++
      (DefaultBsonCustomStringValidator ++ CNilInterchangeFormatValidator[BSONValue](): InterchangeFormatValidator[CustomStringCoproduct, BSONValue])

  /** Uses the default encoder interpreter for BSON */
  object DefaultBsonCustomStringEncoder extends CustomStringEncoder[BSONValue]  {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] = BsonEncoderInterpreter
  }

  /** Uses the default validator interpreter for BSON */
  object DefaultBsonCustomStringValidator extends CustomStringValidator[BSONValue] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[BSONValue] = BsonValidatorInterpreter
  }

  /** Encoder/Validator which uses default ISO format. */
  object IsoBsonJavaTimeEncoder
    extends BsonJavaTimeEncoder {
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
  }


}
