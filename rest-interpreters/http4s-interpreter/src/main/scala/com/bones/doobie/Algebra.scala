package com.bones.doobie

import io.circe.Json

object Algebra {

  trait InterchangeFormat[A]
  object JsonFormat extends InterchangeFormat[Json]
  case class ProtoBuffFormat[P]() extends InterchangeFormat[P]
  case class AvroFormat[A]() extends InterchangeFormat[A]

  val protoBuffFormat = ProtoBuffFormat[String]()
  val avroFormat = AvroFormat[String]

}
