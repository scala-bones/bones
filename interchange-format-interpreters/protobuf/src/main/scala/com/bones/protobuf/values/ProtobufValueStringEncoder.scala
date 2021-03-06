package com.bones.protobuf.values

import com.bones.data.values.CustomStringValue
import com.bones.protobuf.{
  EncodeToProto,
  ProtobufEncoderValue,
  ProtobufSequentialEncoderInterpreter
}

object ProtobufValueStringEncoder extends ProtobufEncoderValue[CustomStringValue] {

  /**
    *  Encode using the same mechanism as string encoder.
    */
  override def encodeToProto[A](alg: CustomStringValue[A]): EncodeToProto[A] =
    ProtobufSequentialEncoderInterpreter.stringData.asInstanceOf[EncodeToProto[A]]

}
