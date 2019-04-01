package com.bones.proto

import com.bones.Schemas
import com.bones.protobuf.ProtoFileInterpreter
import org.scalatest.FunSuite

class ProtbufFileInterpreterTest extends FunSuite {

  val result = ProtoFileInterpreter.fromSchema(Schemas.creditCardSchema)

  val str = ProtoFileInterpreter.messageToProtoFile(result)

  println(str)

}
