package com.bones.protobuf.values

import com.bones.data.values._
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.protobuf.{
  EncodeToProto,
  ProtobufEncoderValue,
  ProtobufSequentialEncoderInterpreter
}

object ExtractionErrorProtoEncoder extends ProtobufEncoderValue[ExtractionErrorValue] {
  val encoder: ProtobufSequentialEncoderInterpreter[ScalaCoreValue] =
    new ProtobufSequentialEncoderInterpreter[ScalaCoreValue] {
      override def customInterpreter: ProtobufEncoderValue[ScalaCoreValue] =
        ProtobufScalaCoreEncoder
    }
  val scalaCoreCustomInterpreter = ProtobufScalaCoreEncoder

  override def encodeToProto[A](alg: ExtractionErrorValue[A]): EncodeToProto[A] =
    alg match {
      case CanNotConvertData =>
        encoder.valueDefinition(ExtractionErrorEncoder.canNotConvertSchema.asValue)
      case NotFoundData =>
        encoder.valueDefinition(ExtractionErrorEncoder.notFoundDataSchema.asValue)
      case ParsingErrorData =>
        encoder.valueDefinition(ExtractionErrorEncoder.parsingErrorSchema.asValue)
      case RequiredValueData =>
        encoder.valueDefinition(ExtractionErrorEncoder.requiredValueSchema.asValue)
      case SumTypeErrorData =>
        encoder.valueDefinition(ExtractionErrorEncoder.sumTypeErrorSchema.asValue)
      case SystemErrorData =>
        encoder.valueDefinition(ExtractionErrorEncoder.systemErrorSchema.asValue)
      case ValidationErrorData =>
        encoder.valueDefinition(ExtractionErrorEncoder.validationErrorSchema.asValue)
      case WrongTypeErrorData =>
        encoder.valueDefinition(ExtractionErrorEncoder.wrongTypeErrorSchema.asValue)
    }
}
