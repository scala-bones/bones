package com.bones.protobuf.custom

import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.protobuf.ProtoFileGeneratorInterpreter
import com.bones.protobuf.ProtoFileGeneratorInterpreter.{Name, stringMessageField}

trait JavaUtilProtoFileInterpreter
    extends ProtoFileGeneratorInterpreter.CustomInterpreter[JavaUtilValue] {
  override def toMessageField[A](alg: JavaUtilValue[A]): (Name, Int) => (
    ProtoFileGeneratorInterpreter.MessageField,
    Vector[ProtoFileGeneratorInterpreter.NestedType],
    Int) =
    alg match {
      case _: UuidData =>
        (name, index) =>
          stringMessageField(name, index)
    }
}
