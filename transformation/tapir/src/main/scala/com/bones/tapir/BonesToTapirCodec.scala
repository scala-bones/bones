package com.bones.tapir

import com.bones.data.Error._
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator
import sttp.tapir
import sttp.tapir.DecodeResult._
import sttp.tapir.ValidationError.Custom
import sttp.tapir.{Codec, CodecFormat, DecodeResult, FieldName, Schema, ValidationError}

object BonesToTapirCodec {

  def encoderValidatorToTapirCodec[ALG[_], A](
    tapirSchema: Schema[A],
    enc: Encoder[ALG, A, String],
    bonesValidator: Validator[String, ALG, A, String]
  ): Codec[String, A, CodecFormat.Json] = {

    new Codec[String, A, CodecFormat.Json] {

      override def schema: Schema[A] = tapirSchema

      override def format: CodecFormat.Json = CodecFormat.Json()

      override def rawDecode(l: String): DecodeResult[A] = {
        bonesValidator.validate(l) match {
          case Left(err) => collectFailures(err)
          case Right(a)  => DecodeResult.Value(a)
        }
      }

      override def encode(h: A): String = enc.encode(h)

      override def validator: tapir.Validator[A] = tapir.Validator.Any[A](List.empty)
    }

  }

  def collectFailures(errors: List[ExtractionError[String]]): Failure = {
    val tapirErrors = errors.map(bonesErrorToTapirError)
    val (validationErrors, failures) =
      tapirErrors.foldLeft((List.empty[ValidationError[_]], List.empty[Failure]))(
        (result, next) => {
          next match {
            case Left(ve) => (ve :: result._1, result._2)
            case Right(f) => (result._1, f :: result._2)
          }
        }
      )

    val failuresWithErrors =
      if (validationErrors.isEmpty) failures
      else InvalidValue(validationErrors) :: failures

    failuresWithErrors match {
      case head :: Nil => head
      case _           => Multiple(failuresWithErrors)
    }

  }

  def bonesErrorToTapirError(be: ExtractionError[String]): Either[ValidationError[_], Failure] = {
    be match {
      case ve: com.bones.data.Error.ValidationError[String, t] =>
        Left(Custom(ve.input, ve.failurePoint.defaultError(ve.input), ve.path.map(FieldName(_))))
      case wt: WrongTypeError[String, t] =>
        Left(
          Custom(
            "wrongType",
            s"Expected type: ${wt.expectedType} provided type: ${wt.providedType}",
            wt.path.map(FieldName(_))
          )
        )
      case cc: CanNotConvert[String, a, t] =>
        Left(
          Custom(
            cc.input,
            s"Can not convert input to type ${cc.toType.getSimpleName}",
            cc.path.map(FieldName(_))
          )
        )
      case rv: RequiredValue[String] =>
        Left(
          Custom(
            "requiredValue",
            s"Missing Required Value ${rv.typeName}",
            rv.path.map(FieldName(_))
          )
        )
      case st: SumTypeError[String] =>
        Left(
          Custom(
            "coproduct type error",
            s"Error In Processing Coproduct type :${st.problem}",
            st.path.map(FieldName(_))
          )
        )
      case pe: ParsingError[String] =>
        pe.throwable match {
          case Some(th) => Right(Error("parsingError", th))
          case None =>
            Left(
              Custom(
                "",
                s"Could not parse input as JSON. ${pe.message}",
                List.empty
              )
            )
        }
      case se: SystemError[String] =>
        Right(Error(s"System Error: ${se.message}", se.th))
      case nf: NotFound[String, id] =>
        Right(Missing)
    }

  }

}
