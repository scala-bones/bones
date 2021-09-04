package com.bones.bson.values

import com.bones.data.values.CustomStringValue
import com.bones.interpreter.encoder.{Encoder, InterchangeFormatEncoderValue}
import reactivemongo.bson.{BSONString, BSONValue}

trait BsonCustomStringEncoder extends InterchangeFormatEncoderValue[CustomStringValue, BSONValue] {
  override def generateEncoder[A](
    alg: CustomStringValue[A]
  ): Encoder[CustomStringValue, A, BSONValue] =
    input => BSONString(input.asInstanceOf[String])
}
