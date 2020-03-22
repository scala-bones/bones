package com.bones.protobuf.custom

import com.bones.data.custom.CustomStringValue
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.{ComputeSize, Encode, EncodeToProto}

trait CustomStringEncoderInterpreter extends ProtobufSequentialEncoderInterpreter.CustomInterpreter[CustomStringValue] {

  /**
    *  Encode using thee same mechansim as string.
    */
  override def encodeToProto[A](alg: CustomStringValue[A]): EncodeToProto[A] =
    ProtobufSequentialEncoderInterpreter.stringData.asInstanceOf[EncodeToProto[A]]

}
