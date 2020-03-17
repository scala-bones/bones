package com.bones.bson

import java.time.format.DateTimeFormatter

import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator}
import reactivemongo.bson.BSONValue

package object custom {


  object BsonCustomStringEncoderAndValidator extends CustomStringEncoder[BSONValue] with CustomStringValidator[BSONValue] {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[BSONValue] = BsonEncoderInterpreter
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[BSONValue] = BsonValidatorInterpreter
  }

  /** Encoder/Validator which uses default ISO format. */
  object BsoJavaTimeEncoder
    extends BsonJavaTimeEncoder
      with BsonJavaTimeValidator {

    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  }

}
