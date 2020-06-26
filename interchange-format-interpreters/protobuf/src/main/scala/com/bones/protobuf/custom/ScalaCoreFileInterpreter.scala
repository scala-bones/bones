package com.bones.protobuf.custom

import com.bones.data.custom.{BigDecimalData, BooleanData, ByteArrayData, DoubleData, EnumerationData, FloatData, IntData, LongData, ScalaCoreValue, ShortData, StringData}
import com.bones.protobuf.ProtoFileGeneratorInterpreter
import com.bones.protobuf.ProtoFileGeneratorInterpreter.{Name, booleanMessageField, byteArrayMessageField, doubleMessageField, floatMessageField, intMessageField, longMessageField, stringMessageField}

trait ScalaCoreFileInterpreter extends ProtoFileGeneratorInterpreter.CustomInterpreter[ScalaCoreValue] {
  override def toMessageField[A](alg: ScalaCoreValue[A]): (Name, Int) => (ProtoFileGeneratorInterpreter.MessageField, Vector[ProtoFileGeneratorInterpreter.NestedType], Int) =
    alg match {
      case ob: BooleanData       => (name, index) => booleanMessageField(name, index)
      case rs: StringData        => (name, index) => stringMessageField(name, index)
      case df: ShortData         => (name, index) => intMessageField(name, index)
      case id: IntData           => (name, index) => intMessageField(name, index)
      case ri: LongData          => (name, index) => longMessageField(name, index)
      case fd: FloatData         => (name, index) => floatMessageField(name, index)
      case fd: DoubleData        => (name, index) => doubleMessageField(name, index)
      case bd: BigDecimalData    => (name, index) => stringMessageField(name, index)
      case ba: ByteArrayData     => (name, index) => byteArrayMessageField(name, index)
      case esd: EnumerationData[e, a] => (name, index) => stringMessageField(name, index)
    }
}
