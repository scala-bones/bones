package com.bones.protobuf.custom

import com.bones.data.custom._
import com.bones.interpreter.custom.ExtractionErrorEncoder
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.{CustomEncoderInterpreter, EncodeToProto}
import com.bones.protobuf.ProtobufUtcSequentialEncoderAndValidator

object ExtractionErrorProtoEncoder extends CustomEncoderInterpreter[ExtractionErrorValue] {
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
