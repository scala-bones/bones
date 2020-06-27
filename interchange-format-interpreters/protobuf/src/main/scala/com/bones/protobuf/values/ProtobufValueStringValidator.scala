package com.bones.protobuf.values

import com.bones.data.values.CustomStringValue
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.{ExtractFromProto, stringData}
import com.bones.protobuf.ProtobufValueValidator

object ProtobufValueStringValidator extends ProtobufValueValidator[CustomStringValue] {

  override def extractFromProto[A](alg: CustomStringValue[A]): ExtractFromProto[A] = {
    stringData[CustomStringValue](
      alg.asInstanceOf[CustomStringValue[String]],
      alg.customValidation :: alg.validations)
      .asInstanceOf[ExtractFromProto[A]]
  }
}
