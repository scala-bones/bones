package com.bones.proto

import com.bones.protobuf.ProtoFileGeneratorInterpreter
import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class ProtbufFileInterpreterTest extends AnyFunSuite {

  val result = ProtoFileGeneratorInterpreter.fromSchemaCustomAlgebra(Schemas.creditCardSchema, com.bones.protobuf.values.defaultProtoFileGenerators)

  val str = ProtoFileGeneratorInterpreter.messageToProtoFile(result)

//  println(str)

}
