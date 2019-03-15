package com.bones.doobie

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
  case class ProtoBuffFormat[P]() extends InterchangeFormat[P] {
    val contentType = "application/protobuf"
  }
  case class AvroFormat[A]() extends InterchangeFormat[A] {
    val contentType = "application/avro"
  }

  val protoBuffFormat = ProtoBuffFormat[String]()
  val avroFormat = AvroFormat[String]

}
