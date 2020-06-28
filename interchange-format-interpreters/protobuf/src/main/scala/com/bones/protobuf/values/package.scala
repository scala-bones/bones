package com.bones.protobuf

import java.time.ZoneOffset

import com.bones.data.values.DefaultValues
import com.bones.protobuf.ProtoFileGeneratorInterpreter.CustomInterpreter.CNilProtoFileCustomInterpreterEncoder
import com.bones.protobuf.ProtobufEncoderValue.CNilProtobufValueEncoder
import com.bones.protobuf.ProtobufValidatorValue.CNilProtobufValueValidator

package object values {

  def defaultEncoders[A]: ProtobufEncoderValue[DefaultValues] =
    ProtobufScalaCoreEncoder ++
      (ProtobufValueStringEncoder ++
        (ProtobufUtcJavaTimeEncoder ++
          (ProtobufJavaUtilEncoder ++ CNilProtobufValueEncoder)))

  val defaultValidators: ProtobufValidatorValue[DefaultValues] =
    ProtobufScalaCoreValidator ++
      (ProtobufValueStringValidator ++
        (ProtobufUtcJavaTimeValidator ++
          (ProtobufJavaUtilValidator ++ CNilProtobufValueValidator)))

  val defaultProtoFileGenerators: ProtoFileGeneratorInterpreter.CustomInterpreter[DefaultValues] =
    ScalaCoreProtoFile ++
      (CustomStringProtoFileInterpreter ++
        (JavaTimeProtoFileInterpreter ++
          (JavaUtilProtoFile ++ CNilProtoFileCustomInterpreterEncoder)))

  object ProtobufScalaCoreEncoder extends ScalaCoreEncoder {
    override val defaultEncoder: ProtobufSequentialEncoderInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
  }

  object ProtobufScalaCoreValidator extends ScalaCoreValidator

  object ScalaCoreProtoFile extends ScalaCoreFileInterpreter

  object ProtobufJavaUtilEncoder extends JavaUtilEncoder

  object ProtobufJavaUtilValidator extends JavaUtilValidator

  object JavaUtilProtoFile extends JavaUtilProtoFileInterpreter

  object ProtobufUtcJavaTimeEncoder extends JavaTimeEncoder {
    override val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
    override val zoneOffset: ZoneOffset = ZoneOffset.UTC
  }

  object ProtobufUtcJavaTimeValidator extends JavaTimeValidator {
    override val coreProtobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
    override val defaultZoneOffset: ZoneOffset = ZoneOffset.UTC
  }

}
