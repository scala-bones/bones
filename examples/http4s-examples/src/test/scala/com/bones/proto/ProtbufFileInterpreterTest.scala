package com.bones.proto

import com.bones.protobuf.ProtoFileGeneratorInterpreter
import com.bones.schemas.Schemas
import org.scalatest.FunSuite

class ProtbufFileInterpreterTest extends FunSuite {

  val result = ProtoFileGeneratorInterpreter.fromSchema(Schemas.creditCardSchema)

  val str = ProtoFileGeneratorInterpreter.messageToProtoFile(result)

//  println(str)

}
