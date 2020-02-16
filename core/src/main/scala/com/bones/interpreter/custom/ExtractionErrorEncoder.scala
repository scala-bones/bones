package com.bones.interpreter.custom

import com.bones.data.Error._
import com.bones.data.custom._
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.{InterchangeFormatEncoder, NoAlgebraEncoder}
import com.bones.syntax._
import shapeless.{::, HNil}
import shapeless.syntax.std.tuple._

object ExtractionErrorEncoder {

  def canNotConvertToHList(canNotConvert: CanNotConvert[_, _]): String :: String :: String :: Option[String] :: HNil = {
    val path = canNotConvert.path.mkString(".")
    val input = canNotConvert.input.toString
    val expectedType = canNotConvert.toType.getCanonicalName
    val stack = canNotConvert.cause.map(_.getStackTrace.toSeq.map(_.toString).mkString("\n"))
    path :: input :: expectedType :: stack :: HNil
  }

  val canNotConvertSchema =
    (
      ("path", string) :<:
        ("input", string) :<:
        ("expectedType", string) :<:
        ("stackTrace", string.optional) :<:
        kvpNil
      ).xmap[CanNotConvert[_, _]](h => sys.error("Mapping to an ExtractionError is not supported"), canNotConvertToHList)

  def notFoundToHList(notFoundData: NotFound[_]): String :: String :: String :: HNil = {
    val path = notFoundData.path.mkString(".")
    val id = notFoundData.id.toString
    val entityName = notFoundData.entityName
    id :: entityName :: path :: HNil
  }

  val notFoundDataSchema =
    (//  case class NotFound[ID](id: ID, entityName: String, path: List[String]) extends ExtractionError

      ("id", string) :<:
        ("entityName", string) :<:
        ("path", string) :<:
        kvpNil
      ).xmap[NotFound[_]](h => sys.error("Mapping to an ExtractionError is not supported"), notFoundToHList)

  def parsingErrorToHList(parsingError: ParsingError): String :: Option[String] :: HNil = {
    parsingError.message :: parsingError.throwable.map(_.getStackTrace.mkString("\n")) :: HNil
  }

  val parsingErrorSchema =
    (
      ("message", string) :<:
        ("stacktrace", string.optional) :<:
        kvpNil
      ).xmap[ParsingError](h => sys.error("Mapping to an ParsingError is not supported"), parsingErrorToHList)


  def sumTypeErrorToHList(sumTypeError: SumTypeError): String :: String :: HNil = {
    sumTypeError.path.mkString(".") :: sumTypeError.problem :: HNil
  }

  val sumTypeErrorSchema =
    (
      ("path", string) :<:
        ("problem", string) :<:
        kvpNil
      ).xmap[SumTypeError](_ => sys.error("Mapping to a SumTypeError is not supported"), sumTypeErrorToHList)

  def systemErrorToHList(systemError: SystemError): String :: String :: String :: Option[String] :: HNil = {
    systemError.path.mkString(".") :: systemError.th.getMessage :: systemError.th.getStackTrace.mkString("\n") :: systemError.message :: HNil
  }

  val systemErrorSchema =
    (
      ("path", string) :<:
        ("errorMessage", string) :<:
        ("stackTrace", string) :<:
        ("message", string.optional) :<:
        kvpNil
      ).xmap[SystemError](_ => sys.error("Mapping to a SystemError is not supported"), systemErrorToHList)

  def validationErrorToHList(validationError: ValidationError[_]): String :: String :: HNil = {
    validationError.path.mkString(".") ::
//      validationError.failurePoint.defaultError(validationError.input) ::
      validationError.failurePoint.description ::
      HNil
  }

  val validationErrorSchema =
    (
      ("path", string) :<:
//        ("errorMessage", string) :<:
        ("validationDescription", string) :<:
        kvpNil
      ).xmap[ValidationError[_]](_ => sys.error("Mapping to a ValidationError is not supported"), validationErrorToHList)

  def wrongTypeToHList(wrongTypeError: WrongTypeError[_]): String :: String :: String :: Option[String] :: Option[String] :: HNil = {
    wrongTypeError.path.mkString(".") ::
      wrongTypeError.providedType.getSimpleName ::
      wrongTypeError.expectedType.getSimpleName ::
    wrongTypeError.cause.map(_.getMessage) ::
    wrongTypeError.cause.map(_.getStackTrace.mkString("\n")) ::
    HNil
  }

  val wrongTypeErrorSchema =
    (
      ("path", string) :<:
        ("providedType", string) :<:
        ("expectedType", string) :<:
        ("errorMessage", string.optional) :<:
        ("errorStackTrace", string.optional) :<:
      kvpNil
    ).xmap[WrongTypeError[_]](_ => sys.error("Mapping to a WrongTypeError is not supported"), wrongTypeToHList)

}

trait ExtractionErrorEncoder[OUT,ALG[_]] extends InterchangeFormatEncoder[ExtractionErrorValue, OUT] {

  import ExtractionErrorEncoder._

  val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  def requiredValueToHList(requiredValue: RequiredValue[_]): String :: String :: HNil = {
    requiredValue.path.mkString(".") :: requiredValue.description :: HNil
  }

  def requiredValueSchema =
    (
      ("path", string) :<:
        ("valueDescription", string) :<:
        kvpNil
      ).xmap[RequiredValue[_]](h => sys.error("Mapping to an RequiredValue is not supported"), requiredValueToHList)


  val canNotConvertEncoder = defaultEncoder
    .encoderFromCustomSchema[NoAlgebra, CanNotConvert[_, _]](canNotConvertSchema, NoAlgebraEncoder[OUT])
  val notFoundEncoder = defaultEncoder
    .encoderFromCustomSchema[NoAlgebra, NotFound[_]](notFoundDataSchema, NoAlgebraEncoder[OUT])
  val parsingErrorEncoder = defaultEncoder
    .encoderFromCustomSchema[NoAlgebra, ParsingError](parsingErrorSchema, NoAlgebraEncoder[OUT])
  def requiredValueEncoder = defaultEncoder
    .encoderFromSchema[RequiredValue[_]](requiredValueSchema)
  val sumTypeErrorEncoder = defaultEncoder
    .encoderFromSchema[SumTypeError](sumTypeErrorSchema)
  val systemErrorEncoder = defaultEncoder
    .encoderFromSchema[SystemError](systemErrorSchema)
  val validationErrorEncoder = defaultEncoder
    .encoderFromSchema[ValidationError[_]](validationErrorSchema)
  val wrongTypeErrorEncoder = defaultEncoder
    .encoderFromSchema[WrongTypeError[_]](wrongTypeErrorSchema)


  override def encode[A](alg: ExtractionErrorValue[A]): A => OUT =
    alg match {
      case CanNotConvertData => canNotConvertEncoder
      case NotFoundData => notFoundEncoder
      case ParsingErrorData => parsingErrorEncoder
      case RequiredValueData => requiredValueEncoder
      case SumTypeErrorData => sumTypeErrorEncoder
      case SystemErrorData => systemErrorEncoder
      case ValidationErrorData => validationErrorEncoder
      case WrongTypeErrorData => wrongTypeErrorEncoder
    }


}
