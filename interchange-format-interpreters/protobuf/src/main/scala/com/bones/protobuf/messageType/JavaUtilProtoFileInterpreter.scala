package com.bones.protobuf.messageType

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.protobuf.messageType.ProtoFileGeneratorInterpreter.stringMessageField

trait JavaUtilProtoFileInterpreter extends CustomInterpreter[JavaUtilValue] {
  override def toMessageField[A](
    alg: JavaUtilValue[A]
  ): (Name, Int) => (MessageField, Vector[NestedType], Int) =
    alg match {
      case _: UuidData =>
        (name, index) =>
          stringMessageField(name, index)
    }
}
