package com.bones.protobuf.values

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.stringDataFromMap
import com.bones.protobuf.{EncodeToProto, ProtobufEncoderValue}

trait JavaUtilEncoder extends ProtobufEncoderValue[JavaUtilValue] {

  override def encodeToProto[A](alg: JavaUtilValue[A]): EncodeToProto[A] = {
    alg match {
      case uu: UuidData => stringDataFromMap(_.toString)

    }
  }
}
