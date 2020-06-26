package com.bones.protobuf.custom

import com.bones.data.custom._
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter._

trait ScalaCoreEncoderInterpreter extends CustomEncoderInterpreter[ScalaCoreValue] {

  val defaultEncoder: ProtobufSequentialEncoderInterpreter

  override def encodeToProto[A](alg: ScalaCoreValue[A]): EncodeToProto[A] =
    alg match {
      case ob: BooleanData            => booleanData
      case rs: StringData             => stringData
      case id: ShortData              => shortData
      case id: IntData                => intData
      case ri: LongData               => longData
      case fd: FloatData              => floatData
      case dd: DoubleData             => doubleData
      case bd: BigDecimalData         => bigDecimalData
      case ba: ByteArrayData          => byteArrayData
      case esd: EnumerationData[e, a] => enumerationData
    }
}
