package com.bones.data

import java.time.ZonedDateTime
import java.util.UUID

import cats.data.Validated
import cats.free.FreeApplicative
import com.bones.data.Error.{CanNotConvert, ValidationError}
import com.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.{::, Generic, HList, HNil, Nat, Succ}

object Value {

  /** ValueDefinitionOp is the base trait to describe a piece of data which may be
    * a single value or an HList. */
  sealed trait ValueDefinitionOp[A] {
    //lift any ValueDefinition into a FreeApplicative
    def lift: ValueDefinition[A] = ???

    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, A]) = {
      Transform(this, gen.to _, gen.from _)
    }

    def convert[B](fab: A => Either[CanNotConvert[A,B], B], fba: B => A, description: String, validations: List[ValidationOp[B] with ToOptionalValidation[B]]): ConversionData[A,B] = {
      ConversionData[A,B](this, fab, fba, description)
    }

  }

  type ValueDefinition[A] = FreeApplicative[ValueDefinitionOp, A]

  /** Wraps a data definition to mark the field optional */
  case class OptionalValueDefinition[B](valueDefinitionOp: ValueDefinitionOp[B]) extends ValueDefinitionOp[Option[B]]

  /** Syntactic sugar to wrap the data definition in an Optional type.
    * Also a sort of marker interface, if this is mixed in, the field is optional.
    * TODO: This should not extend ValueDefinitionOp[A]
    **/
  trait ToOptionalData[B] { self: ValueDefinitionOp[B] =>
    def toOption: OptionalValueDefinition[B] = OptionalValueDefinition[B](self)
  }

  final case class BooleanData() extends ValueDefinitionOp[Boolean] with ToOptionalData[Boolean]
  final case class DoubleData() extends ValueDefinitionOp[Double] with ToOptionalData[Double]
  final case class EitherData[A, B](definitionA: ValueDefinitionOp[A], definitionB: ValueDefinitionOp[B])
    extends ValueDefinitionOp[Either[A, B]] with ToOptionalData[Either[A, B]]
  final case class IntData() extends ValueDefinitionOp[Int] with ToOptionalData[Int]
  final case class ListData[T, L <: List[T]](tDefinition: ValueDefinitionOp[T])
    extends ValueDefinitionOp[L] with ToOptionalData[L]
  final case class StringData() extends ValueDefinitionOp[String] with ToOptionalData[String]
  final case class BigDecimalFromString() extends ValueDefinitionOp[BigDecimal] with ToOptionalData[BigDecimal]

  import java.time.format.DateTimeFormatter

  final case class DateData(dateFormat: DateTimeFormatter, formatDescription: String)
    extends ValueDefinitionOp[ZonedDateTime] with ToOptionalData[ZonedDateTime]
  final case class UuidData() extends ValueDefinitionOp[UUID] with ToOptionalData[UUID]

  final case class ConversionData[A,B](
                                        from: ValueDefinitionOp[A],
                                        fab: A => Either[CanNotConvert[A,B], B],
                                        fba: B => A, description: String
  ) extends ValueDefinitionOp[B] with ToOptionalData[B]

  final case class EnumerationStringData[A](enumeration: Enumeration) extends ValueDefinitionOp[A] with ToOptionalData[A]
  final case class EnumStringData[A <: Enum[A]:Manifest](enums: List[A]) extends ValueDefinitionOp[A] with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }
  final case class Transform[A:Manifest,B](op: ValueDefinitionOp[B], f: A => B, g: B => A) extends ValueDefinitionOp[A] with ToOptionalData[A] { thisBase =>
    val manifestOfA: Manifest[A] = manifest[A]
  }

  sealed trait KvpGroup[L <: HList, HL <: Nat] extends ValueDefinitionOp[L] with ToOptionalData[L]

  /**
    */
  object KvpNil extends KvpGroup[HNil, Nat._0] {
    /**
      * No need to write this as this becomes identity.
      */
    def :::[P <: HList, PL <: Nat, OUT <: HList, OUTL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: Prepend.Aux[P, HNil, OUT],
      length: Length.Aux[P, PL],
      split: Split.Aux[OUT, PL, P, HNil]
    ): KvpGroupHead[OUT, OUTL, P, PL, HNil, Nat._0] =
      KvpGroupHead[OUT, OUTL, P, PL, HNil, Nat._0](kvp, KvpNil, prepend, split, List.empty)

    def ::[H](v: KeyValueDefinition[H]): KvpSingleValueHead[H, HNil, Nat._0, H :: HNil, Nat._1] =
      KvpSingleValueHead(v, List.empty, this)

  }

  final case class KvpSingleValueHead[H, T <: HList, TL <: Nat, OUT <: H :: T, OUTL <: Nat](
    fieldDefinition: KeyValueDefinition[H],
    validations: List[ValidationOp[OUT]],
    tail: KvpGroup[T, TL]
  ) extends KvpGroup[OUT, Succ[TL]] {

    /**
      *
      * When we combine groups, we want to keep the validations separate, but we want to combine the result.
      *
      * @param kvp
      * @tparam OUT2 New HList which combines L (from this) and P (from others)
      * @tparam P
      */
    def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
            implicit prepend: Prepend.Aux[P, OUT, OUT2],
            split: Split.Aux[OUT2, PL, P, OUT]
    ) = KvpGroupHead[OUT2, OUT2L, P, PL, OUT, Succ[TL]](kvp, this, prepend, split, List.empty)

    /** Not sure if we need prepend and split since we can do to a single element */
    def ::[OUT2 <:HList, P](kvd: KeyValueDefinition[P]) =
      KvpSingleValueHead[P, OUT, Succ[TL], P :: OUT, Succ[Succ[TL]]](kvd, List.empty, this)

    def validate(v: ValidationOp[OUT]): KvpSingleValueHead[H,T,TL,OUT,OUTL] = this.copy(validations = v :: validations)
  }

  /** This is a group of KvpGroup that are grouped and the validations match the entire group.  */
  final case class KvpGroupHead[OUT <: HList, OUTL <: Nat, H <: HList, HL<: Nat, T <: HList, TL <: Nat](
                                                                                                         head: KvpGroup[H, HL],
                                                                                                         tail: KvpGroup[T, TL],
                                                                                                         prepend : Prepend.Aux[H, T, OUT],
                                                                                                         split : Split.Aux[OUT, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
                                                                                                         validations: List[ValidationOp[OUT]]
  ) extends KvpGroup[OUT, OUTL] {
    /**
      *
      * When we combine groups, we want to keep the validations separete, but we want to combine the result.
      *
      * @param kvp
      * @tparam OUT2 New HList which combines L (from this) and P (from others)
      * @tparam P
      */
    def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: Prepend.Aux[P, OUT, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, OUT]
    ): KvpGroup[OUT2, OUT2L] =
      KvpGroupHead[OUT2, OUT2L, P, PL, OUT, OUTL](kvp, this, prepend, split, List.empty)

    def ::[P](kvd: KeyValueDefinition[P]) =
      KvpSingleValueHead[P, OUT, OUTL, P :: OUT, Succ[OUTL]](kvd, List.empty, this)

    def validate(v: ValidationOp[OUT]): KvpGroupHead[OUT,OUTL, H, HL, T, TL] = this.copy(validations = v :: validations)

  }

}

