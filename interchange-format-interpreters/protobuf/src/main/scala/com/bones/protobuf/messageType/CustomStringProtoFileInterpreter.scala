package com.bones.protobuf.messageType

import com.bones.data.values.CustomStringValue
import com.bones.protobuf.messageType.ProtoFileGeneratorInterpreter.stringMessageField

object CustomStringProtoFileInterpreter extends CustomInterpreter[CustomStringValue] {

  override def toMessageField[A](
    alg: CustomStringValue[A]
  ): (Name, Int) => (MessageField, Vector[NestedType], Int) =
    (name, index) => stringMessageField(name, index)
}
