package com.bones.http4s

import net.liftweb.json.JsonAST.JValue

object Algebra {

  trait InterchangeFormat[A]
  case class JsonFormat() extends InterchangeFormat[JValue]
  case class ProtoBuffFormat[P]() extends InterchangeFormat[P]
  case class AvroFormat[A]() extends InterchangeFormat[A]

  val jsonFormat = JsonFormat()
  val protoBuffFormat = ProtoBuffFormat[String]()
  val avroFormat = AvroFormat[String]

}
