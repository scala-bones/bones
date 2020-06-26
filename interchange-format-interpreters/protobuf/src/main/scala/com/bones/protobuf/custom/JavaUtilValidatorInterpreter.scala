package com.bones.protobuf.custom

import com.bones.Util.stringToUuid
import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.{CustomValidatorInterpreter, ExtractFromProto, stringDataWithFlatMap}

trait JavaUtilValidatorInterpreter extends CustomValidatorInterpreter[JavaUtilValue] {
  override def extractFromProto[A](alg: JavaUtilValue[A]): ExtractFromProto[A] =
    alg match {
      case uu: UuidData          => stringDataWithFlatMap(Left(uu), stringToUuid, uu.validations)
    }
}
