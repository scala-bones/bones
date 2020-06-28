package com.bones.protobuf.values

import com.bones.data.values._
import com.bones.interpreter.values.ExtractionErrorEncoder
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.EncodeToProto
import com.bones.protobuf.{ProtobufUtcSequentialEncoderAndValidator, ProtobufEncoderValue}

object ExtractionErrorProtoEncoder extends ProtobufEncoderValue[ExtractionErrorValue] {
  private val encoder = ProtobufUtcSequentialEncoderAndValidator
  val scalaCoreCustomInterpreter = ProtobufScalaCoreEncoder

  override def encodeToProto[A](alg: ExtractionErrorValue[A]): EncodeToProto[A] =
    alg match {
      case CanNotConvertData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.canNotConvertSchema, scalaCoreCustomInterpreter)
      case NotFoundData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.notFoundDataSchema,
          scalaCoreCustomInterpreter)
      case ParsingErrorData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.parsingErrorSchema,
          scalaCoreCustomInterpreter)
      case RequiredValueData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.requiredValueSchema,
          scalaCoreCustomInterpreter)
      case SumTypeErrorData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.sumTypeErrorSchema,
          scalaCoreCustomInterpreter)
      case SystemErrorData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.systemErrorSchema,
          scalaCoreCustomInterpreter)
      case ValidationErrorData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.validationErrorSchema,
          scalaCoreCustomInterpreter)
      case WrongTypeErrorData =>
        encoder.valueDefinition(
          ExtractionErrorEncoder.wrongTypeErrorSchema,
          scalaCoreCustomInterpreter)
    }
}
