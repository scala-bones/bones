package com.bones.bson.values

import com.bones.data.values.CustomStringValue
import com.bones.interpreter.InterchangeFormatEncoderValue
import reactivemongo.bson.{BSONString, BSONValue}

trait BsonCustomStringEncoder extends InterchangeFormatEncoderValue[CustomStringValue, BSONValue]{
  override def encode[A](alg: CustomStringValue[A]): A => BSONValue =
    input => BSONString(input.asInstanceOf[String])
}
