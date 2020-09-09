package com.bones.protobuf.messageType

import com.bones.data.values._
import com.bones.protobuf.messageType.ProtoFileGeneratorInterpreter._

trait ScalaCoreFileInterpreter extends CustomInterpreter[ScalaCoreValue] {
  override def toMessageField[A](
    alg: ScalaCoreValue[A]
  ): (Name, Int) => (MessageField, Vector[NestedType], Int) =
    alg match {
      case ob: BooleanData =>
        (name, index) =>
          booleanMessageField(name, index)
      case rs: StringData =>
        (name, index) =>
          stringMessageField(name, index)
      case df: ShortData =>
        (name, index) =>
          intMessageField(name, index)
      case id: IntData =>
        (name, index) =>
          intMessageField(name, index)
      case ri: LongData =>
        (name, index) =>
          longMessageField(name, index)
      case fd: FloatData =>
        (name, index) =>
          floatMessageField(name, index)
      case fd: DoubleData =>
        (name, index) =>
          doubleMessageField(name, index)
      case bd: BigDecimalData =>
        (name, index) =>
          stringMessageField(name, index)
      case ba: ByteArrayData =>
        (name, index) =>
          byteArrayMessageField(name, index)
      case esd: EnumerationData[e, a] =>
        (name, index) =>
          stringMessageField(name, index)
    }
}
