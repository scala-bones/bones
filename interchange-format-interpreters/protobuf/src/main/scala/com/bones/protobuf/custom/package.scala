package com.bones.protobuf

import com.bones.data.custom.{AllCustomAlgebras, CustomStringCoproduct}
import com.bones.protobuf.ProtoFileGeneratorInterpreter.CustomInterpreter
import com.bones.protobuf.ProtoFileGeneratorInterpreter.CustomInterpreter.CNilProtoFileCustomInterpreterEncoder
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter.CNilCustomEncoder
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter.CNilCustomValidatorEncoder

package object custom {

  def allEncoders[A]: CustomEncoderInterpreter[AllCustomAlgebras] =
    ProtobufUtcJavaTimeEncoder ++
      (CustomStringEncoderInterpreter ++ CNilCustomEncoder: CustomEncoderInterpreter[
        CustomStringCoproduct])

  val allValidators: CustomValidatorInterpreter[AllCustomAlgebras] =
    ProtobufUtcJavaTimeValidator ++
      (CustomStringValidatorInterpreter ++ CNilCustomValidatorEncoder: CustomValidatorInterpreter[
        CustomStringCoproduct])

  val allProtoFiles: ProtoFileGeneratorInterpreter.CustomInterpreter[AllCustomAlgebras] =
    JavaTimeProtoFileInterpreter ++
      (CustomStringProtoFileInterpreter ++ CNilProtoFileCustomInterpreterEncoder: CustomInterpreter[
        CustomStringCoproduct])

  object ProtobufUtcJavaTimeEncoder extends JavaTimeEncoderEncoderInterpreter {
    override val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
  }

  object ProtobufUtcJavaTimeValidator extends JavaTimeValidatorInterpreter {
    override val coreProtobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
  }

}
