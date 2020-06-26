package com.bones.protobuf.custom

import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.{
  CustomEncoderInterpreter,
  EncodeToProto,
  stringDataFromMap
}

trait JavaUtilEncoderInterpreter extends CustomEncoderInterpreter[JavaUtilValue] {

  override def encodeToProto[A](alg: JavaUtilValue[A]): EncodeToProto[A] = {
    alg match {
      case uu: UuidData => stringDataFromMap(_.toString)

    }
  }
}
