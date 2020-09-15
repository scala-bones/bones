package com.bones.interpreter.values

import com.bones.data.Error._
import com.bones.data.Sugar
import com.bones.data.values._
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  KvpInterchangeFormatEncoderInterpreter
}
import shapeless.syntax.std.tuple._
import shapeless.{:+:, ::, CNil, Generic, HNil, Inl, Inr}

/** Define the core dsl components */
object syntax extends Sugar[ScalaCoreValue] with ScalaCoreSugar
import com.bones.interpreter.values.syntax._

object ExtractionErrorEncoder {

  def canNotConvertToHList(
    canNotConvert: CanNotConvert[_, _]): String :: String :: String :: Option[String] :: HNil = {
    val path = canNotConvert.path.mkString(".")
    val input = canNotConvert.input.toString
    val expectedType = canNotConvert.toType.getCanonicalName
    val stack = canNotConvert.cause.map(_.getStackTrace.toSeq.map(_.toString).mkString("\n"))
    path :: input :: expectedType :: stack :: HNil
  }

  val canNotConvertSchema =
    (
      ("path", string) ::
        ("input", string) ::
        ("expectedType", string) ::
        ("stackTrace", string.optional) :<:
        kvpNil
    ).hListXmap[CanNotConvert[_, _]](
      _ => sys.error("Mapping to an ExtractionError is not supported"),
      canNotConvertToHList
    )

  def notFoundToHList(notFoundData: NotFound[_]): String :: String :: String :: HNil = {
    val path = notFoundData.path.mkString(".")
    val id = notFoundData.id.toString
    val entityName = notFoundData.entityName
    id :: entityName :: path :: HNil
  }

  val notFoundDataSchema =
    (
      ("id", string) ::
        ("entityName", string) ::
        ("path", string) ::
        kvpNil
    ).hListXmap[NotFound[_]](
      h => sys.error("Mapping to an ExtractionError is not supported"),
      notFoundToHList)

  def parsingErrorToHList(parsingError: ParsingError): String :: Option[String] :: HNil = {
    parsingError.message :: parsingError.throwable.map(_.getStackTrace.mkString("\n")) :: HNil
  }

  val parsingErrorSchema =
    (
      ("message", string) ::
        ("stacktrace", string.optional) :<:
        kvpNil
    ).hListXmap[ParsingError](
      h => sys.error("Mapping to an ParsingError is not supported"),
      parsingErrorToHList)

  def sumTypeErrorToHList(sumTypeError: SumTypeError): String :: String :: HNil = {
    sumTypeError.path.mkString(".") :: sumTypeError.problem :: HNil
  }

  val sumTypeErrorSchema =
    (
      ("path", string) ::
        ("problem", string) ::
        kvpNil
    ).hListXmap[SumTypeError](
      _ => sys.error("Mapping to a SumTypeError is not supported"),
      sumTypeErrorToHList)

  def systemErrorToHList(
    systemError: SystemError): String :: String :: String :: Option[String] :: HNil = {
    systemError.path.mkString(".") :: systemError.th.getMessage :: systemError.th.getStackTrace
      .mkString("\n") :: systemError.message :: HNil
  }

  val systemErrorSchema =
    (
      ("path", string) ::
        ("errorMessage", string) ::
        ("stackTrace", string) ::
        ("message", string.optional) :<:
        kvpNil
    ).hListXmap[SystemError](
      _ => sys.error("Mapping to a SystemError is not supported"),
      systemErrorToHList(_))

  def validationErrorToHList(validationError: ValidationError[_]): String :: String :: HNil = {
    validationError.path.mkString(".") ::
      validationError.failurePoint.description ::
      HNil
  }

  val validationErrorSchema =
    (
      ("path", string) ::
        ("validationDescription", string) ::
        kvpNil
    ).hListXmap[ValidationError[_]](
      _ => sys.error("Mapping to a ValidationError is not supported"),
      validationErrorToHList)

  def requiredValueHList(requiredValue: RequiredValue[_]): String :: String :: HNil = {
    requiredValue.path.mkString(".") :: requiredValue.typeName :: HNil
  }

  def wrongTypeToHList(wrongTypeError: WrongTypeError[_])
    : String :: String :: String :: Option[String] :: Option[String] :: HNil = {
    wrongTypeError.path.mkString(".") ::
      wrongTypeError.providedType ::
      wrongTypeError.expectedType ::
      wrongTypeError.cause.map(_.getMessage) ::
      wrongTypeError.cause.map(_.getStackTrace.mkString("\n")) ::
      HNil
  }

  val wrongTypeErrorSchema =
    (
      ("path", string(sv.words)) ::
        ("providedType", string) ::
        ("expectedType", string) ::
        ("errorMessage", string.optional) :<:
        ("errorStackTrace", string.optional) :<:
        kvpNil
    ).hListXmap[WrongTypeError[_]](
      _ => sys.error("Mapping to a WrongTypeError is not supported"),
      wrongTypeToHList(_))

  val requiredValueSchema =
    (
      ("path", string(sv.words)) ::
        ("description", string) ::
        kvpNil
    ).hListXmap[RequiredValue[_]](
      _ => sys.error("Mapping to a Required Value is not supported"),
      requiredValueHList)

  type ExtractionErrorGeneric = ValidationError[_] :+:
    WrongTypeError[_] :+:
    CanNotConvert[_, _] :+:
    RequiredValue[_] :+:
    SumTypeError :+:
    ParsingError :+:
    SystemError :+:
    NotFound[_] :+:
    CNil

  val extractionErrorGeneric = new Generic[ExtractionError] {
    override type Repr = ExtractionErrorGeneric

    override def to(t: ExtractionError): ExtractionErrorGeneric =
      t match {
        case v: ValidationError[_]  => Inl(v)
        case w: WrongTypeError[_]   => Inr(Inl(w))
        case c: CanNotConvert[_, _] => Inr(Inr(Inl(c)))
        case r: RequiredValue[_]    => Inr(Inr(Inr(Inl(r))))
        case s: SumTypeError        => Inr(Inr(Inr(Inr(Inl(s)))))
        case p: ParsingError        => Inr(Inr(Inr(Inr(Inr(Inl(p))))))
        case s: SystemError         => Inr(Inr(Inr(Inr(Inr(Inr(Inl(s)))))))
        case n: NotFound[_]         => Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(n))))))))
      }

    override def from(r: ExtractionErrorGeneric): ExtractionError = r match {
      case Inl(v)                                    => v
      case Inr(Inl(w))                               => w
      case Inr(Inr(Inl(c)))                          => c
      case Inr(Inr(Inr(Inl(r))))                     => r
      case Inr(Inr(Inr(Inr(Inl(s)))))                => s
      case Inr(Inr(Inr(Inr(Inr(Inl(p))))))           => p
      case Inr(Inr(Inr(Inr(Inr(Inr(Inl(s)))))))      => s
      case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(n)))))))) => n
      case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(_)))))))) => sys.error("Unreachable Code")
    }
  }

  val extractionErrorSchema =
    validationErrorSchema :+:
      wrongTypeErrorSchema :+:
      canNotConvertSchema :+:
      requiredValueSchema :+:
      sumTypeErrorSchema :+:
      parsingErrorSchema :+:
      systemErrorSchema :+:
      notFoundDataSchema :+:
      kvpCoNil

}

trait ExtractionErrorEncoder[OUT]
    extends InterchangeFormatEncoderValue[ExtractionErrorValue, OUT]
    with ScalaCoreSugar { self =>

  import ExtractionErrorEncoder._

  def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, OUT]
  val scalaCoreInterpreter: ScalaCoreEncoder[OUT]

  def requiredValueToHList(requiredValue: RequiredValue[_]): String :: String :: HNil = {
    requiredValue.path.mkString(".") :: requiredValue.typeName :: HNil
  }

  def requiredValueSchema =
    (
      ("path", string) ::
        ("valueDescription", string) ::
        kvpNil
    ).hListXmap[RequiredValue[_]](
      h => sys.error("Mapping to an RequiredValue is not supported"),
      requiredValueToHList)

  def canNotConvertEncoder =
    defaultEncoder
      .generateEncoder[CanNotConvert[_, _]](canNotConvertSchema)

  def notFoundEncoder =
    defaultEncoder
      .generateEncoder[NotFound[_]](notFoundDataSchema)
  def parsingErrorEncoder =
    defaultEncoder
      .generateEncoder[ParsingError](parsingErrorSchema)
  def requiredValueEncoder =
    defaultEncoder
      .generateEncoder[RequiredValue[_]](requiredValueSchema)
  def sumTypeErrorEncoder =
    defaultEncoder
      .generateEncoder[SumTypeError](sumTypeErrorSchema)
  def systemErrorEncoder =
    defaultEncoder
      .generateEncoder[SystemError](systemErrorSchema)
  def validationErrorEncoder =
    defaultEncoder
      .generateEncoder[ValidationError[_]](validationErrorSchema)
  def wrongTypeErrorEncoder =
    defaultEncoder
      .generateEncoder[WrongTypeError[_]](wrongTypeErrorSchema)

  override def encode[A](alg: ExtractionErrorValue[A]): A => OUT =
    alg match {
      case CanNotConvertData   => canNotConvertEncoder
      case NotFoundData        => notFoundEncoder
      case ParsingErrorData    => parsingErrorEncoder
      case RequiredValueData   => requiredValueEncoder
      case SumTypeErrorData    => sumTypeErrorEncoder
      case SystemErrorData     => systemErrorEncoder
      case ValidationErrorData => validationErrorEncoder
      case WrongTypeErrorData  => wrongTypeErrorEncoder
    }

}
