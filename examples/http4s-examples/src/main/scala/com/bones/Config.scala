package com.bones

import com.bones.bson.values.BsonDefaultValuesBytArrayInterpreter
import com.bones.circe.values.Interpreter.CirceDefaultValuesByteArrayInterpreter
import com.bones.http.common._
import com.bones.http4s.{BinaryJsonContentType, ProtobufContentType}
import com.bones.protobuf.values.ProtobufDefaultValuesBytArrayInterpreter
import org.http4s.MediaType
import org.http4s.headers.`Content-Type`

object Config {

  val defaultContentType =
    Content(`Content-Type`(MediaType.application.json), CirceDefaultValuesByteArrayInterpreter)
  val supportedContentTypes = Set(
    Content(BinaryJsonContentType, BsonDefaultValuesBytArrayInterpreter()),
    Content(ProtobufContentType, ProtobufDefaultValuesBytArrayInterpreter())
  )

  val interpreterConfig = ContentInterpreters(defaultContentType, supportedContentTypes)

}
