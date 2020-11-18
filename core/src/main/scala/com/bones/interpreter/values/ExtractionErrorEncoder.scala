package com.bones.interpreter.values

import com.bones.data.Error._
import com.bones.data.{KvpNil, ListData, Sugar}
import com.bones.data.values._
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  KvpInterchangeFormatEncoderInterpreter
}
import shapeless.syntax.std.tuple._
import shapeless.{:+:, ::, CNil, Generic, HNil, Inl, Inr}

/** Define the core dsl components */
object syntax extends Sugar[String, ScalaCoreValue] with ScalaCoreSugar
import com.bones.interpreter.values.syntax._

object ExtractionErrorEncoder {

  def canNotConvertToHList(canNotConvert: CanNotConvert[String, _, _])
    : String :: String :: String :: Option[String] :: HNil = {
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
    ).hListXmap[CanNotConvert[String, _, _]](
      _ => sys.error("Mapping to an ExtractionError is not supported"),
      canNotConvertToHList
    )

  def notFoundToHList(notFoundData: NotFound[String, _]): String :: String :: String :: HNil = {
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
    ).hListXmap[NotFound[String, _]](
      h => sys.error("Mapping to an ExtractionError is not supported"),
      notFoundToHList)

  def parsingErrorToHList(parsingError: ParsingError[String]): String :: Option[String] :: HNil = {
    parsingError.message :: parsingError.throwable.map(_.getStackTrace.mkString("\n")) :: HNil
  }

  val parsingErrorSchema =
    (
      ("message", string) ::
        ("stacktrace", string.optional) :<:
        kvpNil
    ).hListXmap[ParsingError[String]](
      h => sys.error("Mapping to an ParsingError is not supported"),
      parsingErrorToHList)

  def sumTypeErrorToHList(sumTypeError: SumTypeError[String]): String :: String :: HNil = {
    sumTypeError.path.mkString(".") :: sumTypeError.problem :: HNil
  }

  val sumTypeErrorSchema =
    (
      ("path", string) ::
        ("problem", string) ::
        kvpNil
    ).hListXmap[SumTypeError[String]](
      _ => sys.error("Mapping to a SumTypeError is not supported"),
      sumTypeErrorToHList)

  def systemErrorToHList(
    systemError: SystemError[String]): String :: String :: String :: Option[String] :: HNil = {
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
    ).hListXmap[SystemError[String]](
      _ => sys.error("Mapping to a SystemError is not supported"),
      systemErrorToHList(_))

  def validationErrorToHList(
    validationError: ValidationError[String, _]): String :: String :: HNil = {
    validationError.path.mkString(".") ::
      validationError.failurePoint.description ::
      HNil
  }

  val validationErrorSchema =
    (
      ("path", string) ::
        ("validationDescription", string) ::
        kvpNil
    ).hListXmap[ValidationError[String, _]](
      _ => sys.error("Mapping to a ValidationError is not supported"),
      validationErrorToHList)

  def requiredValueHList(requiredValue: RequiredValue[String]): String :: String :: HNil = {
    requiredValue.path.mkString(".") :: requiredValue.typeName :: HNil
  }

  def wrongTypeToHList(wrongTypeError: WrongTypeError[String, _])
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
    ).hListXmap[WrongTypeError[String, _]](
      _ => sys.error("Mapping to a WrongTypeError is not supported"),
      wrongTypeToHList(_))

  val requiredValueSchema =
    (
      ("path", string(sv.words)) ::
        ("description", string) ::
        kvpNil
    ).hListXmap[RequiredValue[String]](
      _ => sys.error("Mapping to a Required Value is not supported"),
      requiredValueHList)

  type ExtractionErrorGeneric = ValidationError[String, _] :+:
    WrongTypeError[String, _] :+:
    CanNotConvert[String, _, _] :+:
    RequiredValue[String] :+:
    SumTypeError[String] :+:
    ParsingError[String] :+:
    SystemError[String] :+:
    NotFound[String, _] :+:
    CNil

  val extractionErrorGeneric = new Generic[ExtractionError[String]] {
    override type Repr = ExtractionErrorGeneric

    override def to(t: ExtractionError[String]): ExtractionErrorGeneric =
      t match {
        case v: ValidationError[String, _]  => Inl(v)
        case w: WrongTypeError[String, _]   => Inr(Inl(w))
        case c: CanNotConvert[String, _, _] => Inr(Inr(Inl(c)))
        case r: RequiredValue[String]       => Inr(Inr(Inr(Inl(r))))
        case s: SumTypeError[String]        => Inr(Inr(Inr(Inr(Inl(s)))))
        case p: ParsingError[String]        => Inr(Inr(Inr(Inr(Inr(Inl(p))))))
        case s: SystemError[String]         => Inr(Inr(Inr(Inr(Inr(Inr(Inl(s)))))))
        case n: NotFound[String, _]         => Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(n))))))))
      }

    override def from(r: ExtractionErrorGeneric): ExtractionError[String] = r match {
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

  private val extractionErrorCoproductSchema =
    validationErrorSchema :+:
      wrongTypeErrorSchema :+:
      canNotConvertSchema :+:
      requiredValueSchema :+:
      sumTypeErrorSchema :+:
      parsingErrorSchema :+:
      systemErrorSchema :+:
      notFoundDataSchema :+:
      kvpCoNil

  val extractionErrorSchema = ExtractionErrorEncoder.extractionErrorCoproductSchema
    .toSuperclassOf[ExtractionError[String]](
      manifest[ExtractionError[String]],
      ExtractionErrorEncoder.extractionErrorGeneric)

  val extractionErrorSchemaList: ListData[String, ScalaCoreValue, ExtractionError[String]] =
    extractionErrorSchema.asValue.list()

  private val errorResponseHList =
    (
      "errors",
      ListData[String, ScalaCoreValue, ExtractionError[String]](
        Left(extractionErrorSchema.asValue),
        "ExtractionError",
        List.empty)) :<:
      new KvpNil[String, ScalaCoreValue]

  case class ErrorResponse(errors: List[ExtractionError[String]])
  val errorResponseSchema =
    errorResponseHList.convert[ErrorResponse]

}

trait ExtractionErrorEncoder[OUT]
    extends InterchangeFormatEncoderValue[ExtractionErrorValue, OUT]
    with ScalaCoreSugar { self =>

  import ExtractionErrorEncoder._

  def defaultEncoder: KvpInterchangeFormatEncoderInterpreter[ScalaCoreValue, OUT]
  val scalaCoreInterpreter: ScalaCoreEncoder[OUT]

  def requiredValueToHList(requiredValue: RequiredValue[String]): String :: String :: HNil = {
    requiredValue.path.mkString(".") :: requiredValue.typeName :: HNil
  }

  def requiredValueSchema =
    (
      ("path", string) ::
        ("valueDescription", string) ::
        kvpNil
    ).hListXmap[RequiredValue[String]](
      h => sys.error("Mapping to an RequiredValue is not supported"),
      requiredValueToHList)

  def canNotConvertEncoder =
    defaultEncoder
      .generateEncoder[CanNotConvert[String, _, _]](canNotConvertSchema)

  def notFoundEncoder =
    defaultEncoder
      .generateEncoder[NotFound[String, _]](notFoundDataSchema)
  def parsingErrorEncoder =
    defaultEncoder
      .generateEncoder[ParsingError[String]](parsingErrorSchema)
  def requiredValueEncoder =
    defaultEncoder
      .generateEncoder[RequiredValue[String]](requiredValueSchema)
  def sumTypeErrorEncoder =
    defaultEncoder
      .generateEncoder[SumTypeError[String]](sumTypeErrorSchema)
  def systemErrorEncoder =
    defaultEncoder
      .generateEncoder[SystemError[String]](systemErrorSchema)
  def validationErrorEncoder =
    defaultEncoder
      .generateEncoder[ValidationError[String, _]](validationErrorSchema)
  def wrongTypeErrorEncoder =
    defaultEncoder
      .generateEncoder[WrongTypeError[String, _]](wrongTypeErrorSchema)

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
