package com.bones.protobuf

package object custom {

  object ProtobufUtcJavaTimeEncoderAndValidator
    extends JavaTimeValidatorInterpreter
    with JavaTimeEncoderInterpreter
    with JavaTimeProtoFileInterpreter {
    override val coreProtobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter = ProtobufUtcSequentialEncoderAndValidator
    override val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter = ProtobufUtcSequentialEncoderAndValidator
  }


}
