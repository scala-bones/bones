package com.bones.protobuf.values

import com.bones.data.values.CustomStringValue
import com.bones.protobuf.ProtoFileGeneratorInterpreter

object CustomStringProtoFileInterpreter
    extends ProtoFileGeneratorInterpreter.CustomInterpreter[CustomStringValue] {

  import ProtoFileGeneratorInterpreter._

  override def toMessageField[A](alg: CustomStringValue[A]): (Name, Int) => (
    ProtoFileGeneratorInterpreter.MessageField,
    Vector[ProtoFileGeneratorInterpreter.NestedType],
    Int) =
    (name, index) => stringMessageField(name, index)
}
