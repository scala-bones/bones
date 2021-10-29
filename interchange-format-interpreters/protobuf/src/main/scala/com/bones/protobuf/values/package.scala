package com.bones.protobuf

import java.time.ZoneOffset

import com.bones.data.values.{
  CNilF,
  CustomStringValue,
  DefaultValues,
  JavaTimeValue,
  JavaUtilValue,
  ScalaCoreValue
}
import com.bones.protobuf.ProtobufEncoderValue.CNilProtobufValueEncoder
import com.bones.protobuf.ProtobufValidatorValue.CNilProtobufValueValidator
import com.bones.protobuf.messageType.{
  CNilProtoFileCustomInterpreterEncoder,
  CustomInterpreter,
  JavaUtilProtoFileInterpreter
}
import shapeless.:+:

package object values {

//  def defaultEncoders[A]: ProtobufEncoderValue[DefaultValues] =
//    ProtobufScalaCoreEncoder ++
//      (ProtobufValueStringEncoder ++
//        (ProtobufUtcJavaTimeEncoder ++
//          (ProtobufJavaUtilEncoder ++ CNilProtobufValueEncoder)))
//
//  val defaultValidators: ProtobufValidatorValue[DefaultValues] =
//    ProtobufScalaCoreValidator ++
//      (ProtobufValueStringValidator ++
//        (ProtobufUtcJavaTimeValidator ++
//          (ProtobufJavaUtilValidator ++ CNilProtobufValueValidator)))

  // Below is equivalent to the above.  Above compiles in 2.13, below compiles in both 2.12 and 2.13
  // start 2.12

  type JavaUtilValueCo[A] = JavaUtilValue[A] :+: CNilF[A]
  type JavaTimeValueCo[A] = JavaTimeValue[A] :+: JavaUtilValueCo[A]
  type CustomStringValueCo[A] = CustomStringValue[A] :+: JavaTimeValueCo[A]

  val defaultEncoders: ProtobufEncoderValue[DefaultValues] = {
    ProtobufEncoderValue.merge[ScalaCoreValue, CustomStringValueCo](
      ProtobufScalaCoreEncoder,
      ProtobufEncoderValue.merge[CustomStringValue, JavaTimeValueCo](
        ProtobufValueStringEncoder,
        ProtobufEncoderValue.merge[JavaTimeValue, JavaUtilValueCo](
          ProtobufUtcJavaTimeEncoder,
          ProtobufEncoderValue
            .merge[JavaUtilValue, CNilF](ProtobufJavaUtilEncoder, CNilProtobufValueEncoder)
        )
      )
    )
  }

  val defaultValidators: ProtobufValidatorValue[DefaultValues] = {
    ProtobufValidatorValue.merge[ScalaCoreValue, CustomStringValueCo](
      ProtobufScalaCoreValidator,
      ProtobufValidatorValue.merge[CustomStringValue, JavaTimeValueCo](
        ProtobufValueStringValidator,
        ProtobufValidatorValue.merge[JavaTimeValue, JavaUtilValueCo](
          ProtobufUtcJavaTimeValidator,
          ProtobufValidatorValue
            .merge[JavaUtilValue, CNilF](ProtobufJavaUtilValidator, CNilProtobufValueValidator)
        )
      )
    )
  }
  // end 2.12

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
