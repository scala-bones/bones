package com.bones.data.values

import com.bones.data.Error._
import com.bones.data.{KvpCoproductCollectionHead, KvpWrappedCoproduct, Sugar}
import shapeless.{:+:, CNil, _}
sealed abstract class ExtractionErrorValue[T]

case object CanNotConvertData extends ExtractionErrorValue[CanNotConvert[_, _]]
case object NotFoundData extends ExtractionErrorValue[NotFound[_]]
case object ParsingErrorData extends ExtractionErrorValue[ParsingError]
case object RequiredValueData extends ExtractionErrorValue[RequiredValue]
case object SumTypeErrorData extends ExtractionErrorValue[SumTypeError]
case object SystemErrorData extends ExtractionErrorValue[SystemError]
case object ValidationErrorData extends ExtractionErrorValue[ValidationError[_]]
case object WrongTypeErrorData extends ExtractionErrorValue[WrongTypeError[_]]

/**
  * Provides convenience methods for creating ExtractionErrorValue types.
  */
trait ExtractionErrorValueSugar extends Sugar[ExtractionErrorValue] {

  val scalaCoreSugar: ScalaCoreSugar
  val sugar: Sugar[ScalaCoreValue]

  private val canNotConvert =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[CanNotConvert[_, _], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: CanNotConvert[_, _]) => "CanNotConvert"
      )

  private val notFound =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[NotFound[_], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: NotFound[_]) => "NotFound")

  private val parsingError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[ParsingError, String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: ParsingError) => "ParsingError")

  private val requiredValue =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[RequiredValue, String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: RequiredValue) => "ParsingError"
      )

  private val sumTypeError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[SumTypeError, String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: SumTypeError) => "SumTypeError"
      )

  private val validationError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[ValidationError[_], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: ValidationError[_]) => "ValidationError"
      )

  private val wrongTypeError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[WrongTypeError[_], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: WrongTypeError[_]) => "WrongTypeError"
      )
  private val systemError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[SystemError, String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: SystemError) => "SystemError"
      )

  type ExtractionErrorCoproduct =
    CanNotConvert[_, _] :+: NotFound[_] :+: ParsingError :+: RequiredValue :+: SumTypeError :+: ValidationError[
      _] :+:
      WrongTypeError[_] :+: SystemError :+: CNil

  implicit val gen: Generic[ExtractionError] { type Repr = ExtractionErrorCoproduct } =
    new Generic[ExtractionError] {
      override type Repr = ExtractionErrorCoproduct

      override def to(t: ExtractionError): ExtractionErrorCoproduct =
        t match {
          case c: CanNotConvert[_, _] => Inl(c)
          case n: NotFound[_]         => Inr(Inl(n))
          case p: ParsingError        => Inr(Inr(Inl(p)))
          case r: RequiredValue       => Inr(Inr(Inr(Inl(r))))
          case s: SumTypeError        => Inr(Inr(Inr(Inr(Inl(s)))))
          case v: ValidationError[_]  => Inr(Inr(Inr(Inr(Inr(Inl(v))))))
          case w: WrongTypeError[_]   => Inr(Inr(Inr(Inr(Inr(Inr(Inl(w)))))))
          case s: SystemError         => Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(s))))))))
        }

      override def from(r: ExtractionErrorCoproduct): ExtractionError =
        r match {
          case Inl(c)                                    => c
          case Inr(Inl(n))                               => n
          case Inr(Inr(Inl(p)))                          => p
          case Inr(Inr(Inr(Inl(r))))                     => r
          case Inr(Inr(Inr(Inr(Inl(s)))))                => s
          case Inr(Inr(Inr(Inr(Inr(Inl(v))))))           => v
          case Inr(Inr(Inr(Inr(Inr(Inr(Inl(w)))))))      => w
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(s)))))))) => s
          case Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inr(_)))))))) => sys.error("Unreachable code")
        }
    }

  implicit val genAux: Generic.Aux[ExtractionError, ExtractionErrorCoproduct] = gen

  val kvpCoproduct
    : KvpCoproductCollectionHead[ScalaCoreValue, _, _, ExtractionErrorCoproduct] = (canNotConvert :+: notFound :+: parsingError :+: requiredValue :+: sumTypeError :+:
    validationError :+: wrongTypeError :+: systemError :+: sugar.kvpCoNil)

  val extractionErrors
    : KvpWrappedCoproduct[ScalaCoreValue, ExtractionError, ExtractionErrorCoproduct] =
    kvpCoproduct
      .toSuperclassOf[ExtractionError]()(manifest[ExtractionError], genAux)

}
