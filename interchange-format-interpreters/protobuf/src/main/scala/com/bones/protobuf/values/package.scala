package com.bones.protobuf

import java.time.ZoneOffset

import com.bones.data.values.{DefaultValues, JavaTimeValue}
import com.bones.protobuf.ProtobufEncoderValue.CNilProtobufValueEncoder
import com.bones.protobuf.ProtobufValidatorValue.CNilProtobufValueValidator
import com.bones.protobuf.messageType.{
  CNilProtoFileCustomInterpreterEncoder,
  CustomInterpreter,
  JavaUtilProtoFileInterpreter
}

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

  object ProtobufScalaCoreEncoder extends ScalaCoreEncoder

  object ProtobufScalaCoreValidator extends ScalaCoreValidator

  object ProtobufJavaUtilEncoder extends JavaUtilEncoder

  object ProtobufJavaUtilValidator extends JavaUtilValidator

  object JavaUtilProtoFile extends JavaUtilProtoFileInterpreter

  object ProtobufUtcJavaTimeEncoder extends JavaTimeEncoder {
    override val zoneOffset: ZoneOffset = ZoneOffset.UTC
  }

  object ProtobufUtcJavaTimeValidator extends JavaTimeValidator {
    override val defaultZoneOffset: ZoneOffset = ZoneOffset.UTC
  }

  val defaultUtcValidator = new ProtobufSequentialValidatorInterpreter[DefaultValues] {
    override val customInterpreter: ProtobufValidatorValue[DefaultValues] =
      defaultValidators
    override val zoneOffset: ZoneOffset = ZoneOffset.UTC
  }

  val defaultEncoder = new ProtobufSequentialEncoderInterpreter[DefaultValues] {
    override def customInterpreter: ProtobufEncoderValue[DefaultValues] = defaultEncoders
  }

}
