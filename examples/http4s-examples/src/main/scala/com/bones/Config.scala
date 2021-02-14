package com.bones

import com.bones.bson.values.BsonDefaultValuesBytArrayInterpreter
import com.bones.circe.values.Interpreter.CirceDefaultValuesByteArrayInterpreter
import com.bones.http.common._
import com.bones.protobuf.values.ProtobufDefaultValuesBytArrayInterpreter

object Config {

  val defaultContentType = Content("application/json", CirceDefaultValuesByteArrayInterpreter)
  val supportedContentTypes = Set(
    Content("application/ubjson", BsonDefaultValuesBytArrayInterpreter()),
    Content("application/protobuf", ProtobufDefaultValuesBytArrayInterpreter())
  )

  val interpreterConfig = ContentInterpreters(defaultContentType, supportedContentTypes)

}
