package com.bones.protobuf.values

import com.bones.data.values.CustomStringValue
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.stringData
import com.bones.protobuf.{ExtractFromProto, ProtobufValidatorValue}

object ProtobufValueStringValidator extends ProtobufValidatorValue[CustomStringValue] {

  override def extractFromProto[A](alg: CustomStringValue[A]): ExtractFromProto[A] = {
    stringData[CustomStringValue](
      alg.asInstanceOf[CustomStringValue[String]],
      alg.customValidation :: alg.validations)
      .asInstanceOf[ExtractFromProto[A]]
  }
}
