package com.bones.proto

import com.bones.protobuf.ProtoFileInterpreter
import com.bones.schemas.Schemas
import org.scalatest.FunSuite

class ProtbufFileInterpreterTest extends FunSuite {

  val result = ProtoFileInterpreter.fromSchema(Schemas.creditCardSchema)

  val str = ProtoFileInterpreter.messageToProtoFile(result)

//  println(str)

}
