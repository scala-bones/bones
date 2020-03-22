package com.bones.protobuf.custom

import com.bones.data.custom.CustomStringValue
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.{CustomInterpreter, ExtractFromProto, stringData}

trait CustomStringValidatorInterpreter extends CustomInterpreter[CustomStringValue] {

  override def extractFromProto[A](alg: CustomStringValue[A]): ExtractFromProto[A] = {
    stringData[CustomStringValue](Right(alg.asInstanceOf[CustomStringValue[String]]), alg.customValidation :: alg.validations)
      .asInstanceOf[ExtractFromProto[A]]
  }
}
