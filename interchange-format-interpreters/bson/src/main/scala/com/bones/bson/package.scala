package com.bones

import com.bones.interpreter.{InterchangeFormatEncoder, InterchangeFormatValidator}
import reactivemongo.bson.BSONValue

package object bson {
  
  trait BsonValidator[ALG[_]] extends InterchangeFormatValidator[ALG, BSONValue]
  trait BsonEncoder[ALG[_]] extends InterchangeFormatEncoder[ALG, BSONValue]

}
