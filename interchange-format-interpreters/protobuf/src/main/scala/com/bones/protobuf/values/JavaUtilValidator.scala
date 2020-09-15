package com.bones.protobuf.values

import com.bones.Util.stringToUuid
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.stringDataWithFlatMap
import com.bones.protobuf.{ExtractFromProto, ProtobufValidatorValue}

trait JavaUtilValidator extends ProtobufValidatorValue[JavaUtilValue] {
  override def extractFromProto[A](alg: JavaUtilValue[A]): ExtractFromProto[A] =
    alg match {
      case uu: UuidData => stringDataWithFlatMap(uu.typeName, stringToUuid, uu.validations)
    }
}
