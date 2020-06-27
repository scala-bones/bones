package com.bones.protobuf.values

import com.bones.data.values.CustomStringValue
import com.bones.protobuf.{ProtobufSequentialEncoderInterpreter, ProtobufValueEncoder}
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.EncodeToProto

object ProtobufValueStringEncoder extends ProtobufValueEncoder[CustomStringValue] {

  /**
    *  Encode using the same mechanism as string encoder.
    */
  override def encodeToProto[A](alg: CustomStringValue[A]): EncodeToProto[A] =
    ProtobufSequentialEncoderInterpreter.stringData.asInstanceOf[EncodeToProto[A]]

}
