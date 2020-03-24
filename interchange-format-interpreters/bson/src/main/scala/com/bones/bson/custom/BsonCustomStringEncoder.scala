package com.bones.bson.custom

import com.bones.data.custom.CustomStringValue
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import reactivemongo.bson.{BSONString, BSONValue}

trait BsonCustomStringEncoder extends InterchangeFormatEncoder[CustomStringValue, BSONValue]{
  override def encode[A](alg: CustomStringValue[A]): A => BSONValue =
    input => BSONString(input.asInstanceOf[String])
}
