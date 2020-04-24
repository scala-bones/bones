package com.bones.protobuf.custom

import com.bones.data.custom._
import com.bones.interpreter.custom.ExtractionErrorEncoder
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.{CustomEncoderInterpreter, EncodeToProto, NoAlgebraCustomEncoderInterpreter}
import com.bones.protobuf.ProtobufUtcSequentialEncoderAndValidator

object ExtractionErrorProtoEncoder extends CustomEncoderInterpreter[ExtractionErrorValue] {
  private val encoder = ProtobufUtcSequentialEncoderAndValidator


  override def encodeToProto[A](alg: ExtractionErrorValue[A]): EncodeToProto[A] =
    alg match {
      case CanNotConvertData => encoder.valueDefinition(ExtractionErrorEncoder.canNotConvertSchema, NoAlgebraCustomEncoderInterpreter)
      case NotFoundData => encoder.valueDefinition(ExtractionErrorEncoder.notFoundDataSchema, NoAlgebraCustomEncoderInterpreter)
      case ParsingErrorData => encoder.valueDefinition(ExtractionErrorEncoder.parsingErrorSchema, NoAlgebraCustomEncoderInterpreter)
      case RequiredValueData => encoder.valueDefinition(ExtractionErrorEncoder.requiredValueSchema, NoAlgebraCustomEncoderInterpreter)
      case SumTypeErrorData => encoder.valueDefinition(ExtractionErrorEncoder.sumTypeErrorSchema, NoAlgebraCustomEncoderInterpreter)
      case SystemErrorData => encoder.valueDefinition(ExtractionErrorEncoder.systemErrorSchema, NoAlgebraCustomEncoderInterpreter)
      case ValidationErrorData => encoder.valueDefinition(ExtractionErrorEncoder.validationErrorSchema, NoAlgebraCustomEncoderInterpreter)
      case WrongTypeErrorData => encoder.valueDefinition(ExtractionErrorEncoder.wrongTypeErrorSchema, NoAlgebraCustomEncoderInterpreter)
    }
}
