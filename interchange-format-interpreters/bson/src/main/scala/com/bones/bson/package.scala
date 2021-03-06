package com.bones

import com.bones.data.Error.{ExtractionErrors, ParsingError}
import com.bones.interpreter.encoder.InterchangeFormatEncoderValue
import com.bones.interpreter.validator.InterchangeFormatValidatorValue
import reactivemongo.bson.buffer.{ArrayBSONBuffer, ArrayReadableBuffer}
import reactivemongo.bson.{BSONDocument, BSONValue}

import scala.util.Try

package object bson {

  trait BsonValidator[ALG[_]] extends InterchangeFormatValidatorValue[ALG, BSONValue]
  trait BsonEncoder[ALG[_]] extends InterchangeFormatEncoderValue[ALG, BSONValue]

  def fromByteArray(arr: Array[Byte]): Either[ExtractionErrors[String], BSONValue] = {
    val buffer = ArrayReadableBuffer(arr)
    Try {
      BSONDocument.read(buffer)
    }.toEither.left.map(err => List(ParsingError(err.getMessage)))
  }

  def bsonResultToBytes(bsonValue: BSONValue): Array[Byte] = {
    val buffer = new ArrayBSONBuffer()
    BSONDocument.write(bsonValue.asInstanceOf[BSONDocument], buffer)
    buffer.array
  }

}
