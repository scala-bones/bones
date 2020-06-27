package com.bones.protobuf.values

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.{EncodeToProto, stringDataFromMap}
import com.bones.protobuf.ProtobufValueEncoder

trait JavaUtilEncoder extends ProtobufValueEncoder[JavaUtilValue] {

  override def encodeToProto[A](alg: JavaUtilValue[A]): EncodeToProto[A] = {
    alg match {
      case uu: UuidData => stringDataFromMap(_.toString)

    }
  }
}
