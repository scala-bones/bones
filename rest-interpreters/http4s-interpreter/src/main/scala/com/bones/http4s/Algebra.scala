package com.bones.http4s

import io.circe.Json
import reactivemongo.bson.BSONValue

object Algebra {

  trait InterchangeFormat[A] {
    def contentType: String
  }
  object JsonFormat extends InterchangeFormat[Json] {
    val contentType = "application/json"
  }
  object BsonFormat extends InterchangeFormat[BSONValue] {
    val contentType = "application/ubjson"
  }
  object ProtoBuffFormat extends InterchangeFormat[Array[Byte]] {
    val contentType = "application/protobuf"
  }
  object AvroFormat extends InterchangeFormat[Array[Byte]] {
    val contentType = "application/avro"
  }

}
