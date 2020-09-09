package com.bones.proto

import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class ProtbufFileInterpreterTest extends AnyFunSuite {

  val result = com.bones.protobuf.messageType.defaultProtoFile
    .fromSchemaCustomAlgebra(Schemas.creditCardSchema)

  val str = com.bones.protobuf.messageType.defaultProtoFile.messageToProtoFile(result)

//  println(str)

}
