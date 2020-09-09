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
        encoder.fromKvpCollection(ExtractionErrorEncoder.canNotConvertSchema)
      case NotFoundData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.notFoundDataSchema)
      case ParsingErrorData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.parsingErrorSchema)
      case RequiredValueData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.requiredValueSchema)
      case SumTypeErrorData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.sumTypeErrorSchema)
      case SystemErrorData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.systemErrorSchema)
      case ValidationErrorData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.validationErrorSchema)
      case WrongTypeErrorData =>
        encoder.fromKvpCollection(ExtractionErrorEncoder.wrongTypeErrorSchema)
    }
}
