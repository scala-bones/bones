package com.bones.protobuf.custom

import com.bones.data.custom.CustomStringValue
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.EncodeToProto

object CustomStringEncoderInterpreter
    extends ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter[CustomStringValue] {

  /**
    *  Encode using the same mechanism as string encoder.
    */
  override def encodeToProto[A](alg: CustomStringValue[A]): EncodeToProto[A] =
    ProtobufSequentialEncoderInterpreter.stringData.asInstanceOf[EncodeToProto[A]]

}
