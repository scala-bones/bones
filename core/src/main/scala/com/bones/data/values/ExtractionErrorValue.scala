package com.bones.data.values

import com.bones.data.Error._
import com.bones.data.{KvpCoproductCollectionHead, KvpWrappedCoproduct, Sugar}
import shapeless.{:+:, CNil, _}
sealed abstract class ExtractionErrorValue[T]

case object CanNotConvertData extends ExtractionErrorValue[CanNotConvert[String, _, _]]
case object NotFoundData extends ExtractionErrorValue[NotFound[String, _]]
case object ParsingErrorData extends ExtractionErrorValue[ParsingError[String]]
case object RequiredValueData extends ExtractionErrorValue[RequiredValue[String]]
case object SumTypeErrorData extends ExtractionErrorValue[SumTypeError[String]]
case object SystemErrorData extends ExtractionErrorValue[SystemError[String]]
case object ValidationErrorData extends ExtractionErrorValue[ValidationError[String, _]]
case object WrongTypeErrorData extends ExtractionErrorValue[WrongTypeError[String, _]]

/**
  * Provides convenience methods for creating ExtractionErrorValue types.
  */
trait ExtractionErrorValueSugar extends Sugar[ExtractionErrorValue] {

  val scalaCoreSugar: ScalaCoreSugar
  val sugar: Sugar[ScalaCoreValue]

  private val canNotConvert =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[CanNotConvert[String, _, _], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: CanNotConvert[String, _, _]) => "CanNotConvert"
      )

  private val notFound =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[NotFound[String, _], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: NotFound[String, _]) => "NotFound")

  private val parsingError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[ParsingError[String], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: ParsingError[String]) => "ParsingError")

  private val requiredValue =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[RequiredValue[String], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: RequiredValue[String]) => "ParsingError"
      )

  private val sumTypeError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[SumTypeError[String], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: SumTypeError[String]) => "SumTypeError"
      )

  private val validationError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[ValidationError[String, _], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: ValidationError[String, _]) => "ValidationError"
      )

  private val wrongTypeError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[WrongTypeError[String, _], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: WrongTypeError[String, _]) => "WrongTypeError"
      )
  private val systemError =
    (("name", scalaCoreSugar.string) :: sugar.kvpNil)
      .xmap[SystemError[String], String](
        (_: String) =>
          throw new UnsupportedOperationException("Decoding of ExtractionError not supported"),
        (_: SystemError[String]) => "SystemError"
      )

  type ExtractionErrorCoproduct =
    CanNotConvert[String, _, _] :+: NotFound[String, _] :+: ParsingError[String] :+: RequiredValue[
      String] :+:
      SumTypeError[String] :+: ValidationError[String, _] :+:
      WrongTypeError[String, _] :+: SystemError[String] :+: CNil

  implicit val gen: Generic[ExtractionError[String]] { type Repr = ExtractionErrorCoproduct } =
    new Generic[ExtractionError[String]] {
      override type Repr = ExtractionErrorCoproduct

      override def to(t: ExtractionError[String]): ExtractionErrorCoproduct =
        t match {
          case c: CanNotConvert[String, _, _] => Inl(c)
          case n: NotFound[String, _]         => Inr(Inl(n))
          case p: ParsingError[String]        => Inr(Inr(Inl(p)))
          case r: RequiredValue[String]       => Inr(Inr(Inr(Inl(r))))
          case s: SumTypeError[String]        => Inr(Inr(Inr(Inr(Inl(s)))))
          case v: ValidationError[String, _]  => Inr(Inr(Inr(Inr(Inr(Inl(v))))))
          case w: WrongTypeError[String, _]   => Inr(Inr(Inr(Inr(Inr(Inr(Inl(w)))))))
          case s: SystemError[String]         => Inr(Inr(Inr(Inr(Inr(Inr(Inr(Inl(s))))))))
        }

      override def from(r: ExtractionErrorCoproduct): ExtractionError[String] =
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

  implicit val genAux: Generic.Aux[ExtractionError[String], ExtractionErrorCoproduct] = gen

  val kvpCoproduct: KvpCoproductCollectionHead[
    String,
    ScalaCoreValue,
    _,
    _,
    ExtractionErrorCoproduct] = (canNotConvert :+: notFound :+: parsingError :+: requiredValue :+: sumTypeError :+:
    validationError :+: wrongTypeError :+: systemError :+: sugar.kvpCoNil)

  val extractionErrors: KvpWrappedCoproduct[
    String,
    ScalaCoreValue,
    ExtractionError[String],
    ExtractionErrorCoproduct] =
    kvpCoproduct
      .toSuperclassOf[ExtractionError[String]]()(manifest[ExtractionError[String]], genAux)

}
