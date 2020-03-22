package com.bones.bson

import java.time.format.DateTimeFormatter

import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator}
import reactivemongo.bson.BSONValue

package object custom {


  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, BSONValue] =
    IsoBsonJavaTimeEncoder ++
      (BsonCustomStringEncoder ++ CNilInterchangeFormatEncoder[BSONValue](): InterchangeFormatEncoder[CustomStringCoproduct, BSONValue])

  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, BSONValue] =
    IsoBsonJavaTimeValidator ++
      (BsonCustomStringValidator ++ CNilInterchangeFormatValidator[BSONValue](): InterchangeFormatValidator[CustomStringCoproduct, BSONValue])


  object BsonCustomStringEncoder extends CustomStringEncoder[BSONValue]  {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] = BsonEncoderInterpreter
  }

  object BsonCustomStringValidator extends CustomStringValidator[BSONValue] {
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

}
