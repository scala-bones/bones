package com.bones.protobuf

import java.time.ZoneOffset

import com.bones.data.custom.AllCustomAlgebras
import com.bones.protobuf.ProtoFileGeneratorInterpreter.CustomInterpreter.CNilProtoFileCustomInterpreterEncoder
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter.CNilCustomEncoder
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter.CNilCustomValidatorEncoder

package object custom {

  def allEncoders[A]: CustomEncoderInterpreter[AllCustomAlgebras] =
    ProtobufScalaCoreEncoder ++
      (CustomStringEncoderInterpreter ++
        (ProtobufUtcJavaTimeEncoder ++
          (ProtobufJavaUtilEncoder ++ CNilCustomEncoder)))

  val allValidators: CustomValidatorInterpreter[AllCustomAlgebras] =
    ProtobufScalaCoreValidator ++
      (CustomStringValidatorInterpreter ++
        (ProtobufUtcJavaTimeValidator ++
          (ProtobufJavaUtilValidator ++ CNilCustomValidatorEncoder)))

  val allProtoFiles: ProtoFileGeneratorInterpreter.CustomInterpreter[AllCustomAlgebras] =
    ScalaCoreProtoFile ++
      (CustomStringProtoFileInterpreter ++
        (JavaTimeProtoFileInterpreter ++
          (JavaUtilProtoFile ++ CNilProtoFileCustomInterpreterEncoder)))

  object ProtobufScalaCoreEncoder extends ScalaCoreEncoderInterpreter {
    override val defaultEncoder: ProtobufSequentialEncoderInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
  }

  object ProtobufScalaCoreValidator extends ScalaCoreValidatorInterpreter

  object ScalaCoreProtoFile extends ScalaCoreFileInterpreter

  object ProtobufJavaUtilEncoder extends JavaUtilEncoderInterpreter

  object ProtobufJavaUtilValidator extends JavaUtilValidatorInterpreter

  object JavaUtilProtoFile extends JavaUtilProtoFileInterpreter

  object ProtobufUtcJavaTimeEncoder extends JavaTimeEncoderEncoderInterpreter {
    override val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
    override val zoneOffset: ZoneOffset = ZoneOffset.UTC
  }

  object ProtobufUtcJavaTimeValidator extends JavaTimeValidatorInterpreter {
    override val coreProtobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
    override val defaultZoneOffset: ZoneOffset = ZoneOffset.UTC
  }

}
