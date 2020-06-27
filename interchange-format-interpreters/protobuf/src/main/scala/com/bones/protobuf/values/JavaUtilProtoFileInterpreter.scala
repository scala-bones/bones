package com.bones.protobuf.values

import com.bones.data.values.{JavaUtilValue, UuidData}
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
