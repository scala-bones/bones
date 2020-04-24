package com.bones
import java.time.ZoneOffset

package object protobuf {

  object ProtobufUtcSequentialEncoderAndValidator
      extends ProtobufSequentialEncoderInterpreter
      with ProtobufSequentialValidatorInterpreter {
    override val zoneOffset: ZoneOffset = ZoneOffset.UTC
  }

}
