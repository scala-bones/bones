package com.bones

import com.bones.interpreter.{InterchangeFormatEncoderValue, InterchangeFormatValidatorValue}
import reactivemongo.bson.BSONValue

package object bson {
  
  trait BsonValidator[ALG[_]] extends InterchangeFormatValidatorValue[ALG, BSONValue]
  trait BsonEncoder[ALG[_]] extends InterchangeFormatEncoderValue[ALG, BSONValue]

}
