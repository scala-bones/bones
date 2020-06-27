package com.bones.bson.values

import com.bones.data.values.CustomStringValue
import com.bones.interpreter.InterchangeFormatEncoder
import reactivemongo.bson.{BSONString, BSONValue}

trait BsonCustomStringEncoder extends InterchangeFormatEncoder[CustomStringValue, BSONValue]{
  override def encode[A](alg: CustomStringValue[A]): A => BSONValue =
    input => BSONString(input.asInstanceOf[String])
}
