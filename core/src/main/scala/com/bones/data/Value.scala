package com.bones.data

import java.time.ZonedDateTime
import java.util.UUID

import cats.free.FreeApplicative
import com.bones.data.Error.CanNotConvert
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.{::, Generic, HList, HNil, Nat, Succ}
import java.time.format.DateTimeFormatter



object Value {

  object ValueDefinitionOp {
    implicit class StringToEnum(op: ValueDefinitionOp[String]) {
      def enumeration[A: Manifest](
          enumeration: Enumeration): EnumerationStringData[A] =
        EnumerationStringData[A](enumeration, List.empty)

      def enum[A <: Enum[A]: Manifest](enums: List[A]): EnumStringData[A] =
        EnumStringData[A](enums, List.empty)
    }
  }

  /** ValueDefinitionOp is the base trait to describe a piece of data which may be
    * a single value or an HList. */
  sealed abstract class ValueDefinitionOp[A] {

    def asSumType[B](
        description: String,
        fab: (A, List[String]) => Either[CanNotConvert[A, B], B],
        fba: B => A,
        keys: List[A],
        validations: List[ValidationOp[B]]
    ): SumTypeData[A, B] = {
      SumTypeData[A, B](this, fab, fba, keys, validations)
    }
  }

  type ValueDefinition[A] = FreeApplicative[ValueDefinitionOp, A]

  /** Wraps a data definition to mark the field optional */
  case class OptionalValueDefinition[B](valueDefinitionOp: ValueDefinitionOp[B])
      extends ValueDefinitionOp[Option[B]] {}

  /** Syntactic sugar to wrap the data definition in an Optional type. */
  trait ToOptionalData[B] { self: ValueDefinitionOp[B] =>
    val optional: OptionalValueDefinition[B] = OptionalValueDefinition[B](self)
  }

  /** Syntactic sugar to wrap the definition in a List type. */
  trait ToListData[B] { self: ValueDefinitionOp[B] =>
    val list: ListData[B] = ListData[B](self, List.empty)
  }

  final case class BooleanData(validations: List[ValidationOp[Boolean]])
      extends ValueDefinitionOp[Boolean]
      with ToOptionalData[Boolean]
  final case class EitherData[A, B](definitionA: ValueDefinitionOp[A],
                                    definitionB: ValueDefinitionOp[B])
      extends ValueDefinitionOp[Either[A, B]]
      with ToOptionalData[Either[A, B]] {}
  final case class LongData(validations: List[ValidationOp[Long]])
      extends ValueDefinitionOp[Long]
      with ToOptionalData[Long]
  final case class ListData[T](tDefinition: ValueDefinitionOp[T],
                               validations: List[ValidationOp[List[T]]])
      extends ValueDefinitionOp[List[T]]
      with ToOptionalData[List[T]]

  final case class StringData(validations: List[ValidationOp[String]])
      extends ValueDefinitionOp[String]
      with ToOptionalData[String]
  final case class BigDecimalData(validations: List[ValidationOp[BigDecimal]])
      extends ValueDefinitionOp[BigDecimal]
      with ToOptionalData[BigDecimal]
  // base64-encoded characters, for example, U3dhZ2dlciByb2Nrcw==
  final case class ByteArrayData(
      validations: List[ValueDefinitionOp[Array[Byte]]])
      extends ValueDefinitionOp[Array[Byte]]
      with ToOptionalData[Array[Byte]]

  final case class DateTimeData(dateFormat: DateTimeFormatter,
                                formatDescription: String,
                                validations: List[ValidationOp[ZonedDateTime]])
      extends ValueDefinitionOp[ZonedDateTime]
      with ToOptionalData[ZonedDateTime]

  final case class UuidData(validations: List[ValidationOp[UUID]])
      extends ValueDefinitionOp[UUID]
      with ToOptionalData[UUID]

  final case class EnumerationStringData[A: Manifest](
      enumeration: Enumeration,
      validations: List[ValidationOp[A]])
      extends ValueDefinitionOp[A]
      with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }

  final case class EnumStringData[A <: Enum[A]: Manifest](
      enums: List[A],
      validations: List[ValidationOp[A]])
      extends ValueDefinitionOp[A]
      with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }

  final case class KvpGroupData[H <: HList, HL <: Nat](
      kvpGroup: KvpGroup[H, HL],
      validations: List[ValidationOp[H]])
      extends ValueDefinitionOp[H]
      with ToOptionalData[H] {

    def convert[Z: Manifest](validation: ValidationOp[Z]*)(
        implicit gen: Generic.Aux[Z, H]): XMapData[H, HL, Z] =
      XMapData(kvpGroup, gen.from, gen.to, validation.toList)
  }

  trait BonesSchema[A] {
    val manifestOfA: Manifest[A]
  }
  final case class XMapData[A <: HList, AL <: Nat, B: Manifest](
     from: KvpGroup[A, AL],
     fab: A => B,
     fba: B => A,
     validations: List[ValidationOp[B]])
    extends ValueDefinitionOp[B] with ToOptionalData[B] with ToListData[B] with BonesSchema[B]{
    val manifestOfA: Manifest[B] = manifest[B]
  }

  final case class SumTypeData[A, B](
      from: ValueDefinitionOp[A],
      fab: (A, List[String]) => Either[CanNotConvert[A, B], B],
      fba: B => A,
      keys: List[A],
      validations: List[ValidationOp[B]]
  ) extends ValueDefinitionOp[B] {}

  /**
    * Base trait of a ValueDefinition where the value is a list of data.
    * @tparam H
    * @tparam N
    */
  sealed trait KvpGroup[H <: HList, N <: Nat] {

    def convert[A: Manifest](validation: ValidationOp[A]*)(
        implicit gen: Generic.Aux[A, H]): XMapData[H, N, A] =
      XMapData(this, gen.from, gen.to, validation.toList)

    def convert[A: Manifest](
        implicit gen: Generic.Aux[A, H]): XMapData[H, N, A] = convert[A]()

    def xmap[A: Manifest](f: H => A, g: A => H, validations: ValidationOp[A]*) =
      XMapData(this, f, g, validations.toList)

    def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](
        kvp: KvpGroup[HP, NP])(
        implicit prepend: Prepend.Aux[HP, H, HO],
        lengthP: Length.Aux[HP, NP],
        length: Length.Aux[HO, NO],
        split: Split.Aux[HO, NP, HP, H]
    ): KvpGroup[HO, NO]

    def ::[A](v: KeyValueDefinition[A]): KvpSingleValueHead[A, H, N, A :: H]

    /* The ability to prefix an XMapData (case class) to a KvkGroup */
    def :><:[OUT2 <: HList, OUT2L <: Nat, A: Manifest,HX<:HList, NX<:Nat](dc: XMapData[HX, NX, A]):
    KvpXMapDataHead[A, H, N, A :: H,HX,NX] =
      KvpXMapDataHead[A,H,N,A::H,HX,NX](dc, List.empty, this)

    def optional: OptionalKvpGroup[H, N] = OptionalKvpGroup[H, N](this)
  }

  final case class OptionalKvpGroup[H <: HList, HL <: Nat](
      kvpGroup: KvpGroup[H, HL])
      extends KvpGroup[Option[H] :: HNil, Nat._1] {

    override def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](
        kvp: KvpGroup[HP, NP])(
        implicit prepend: hlist.Prepend.Aux[HP, Option[H] :: HNil, HO],
        lengthP: Length.Aux[HP, NP],
        lengthO: Length.Aux[HO, NO],
        split: Split.Aux[HO, NP, HP, Option[H] :: HNil]
    ): KvpGroup[HO, NO] = ???

    override def ::[A](v: KeyValueDefinition[A])
      : KvpSingleValueHead[A,
                           Option[H] :: HNil,
                           Nat._1,
                           A :: Option[H] :: HNil] = ???
  }

  /**
    */
  object KvpNil extends KvpGroup[HNil, Nat._0] {

    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](
        kvp: KvpGroup[P, PL])(
        implicit prepend: hlist.Prepend.Aux[P, HNil, OUT2],
        lengthP: Length.Aux[P, PL],
        length: Length.Aux[OUT2, OUT2L],
        split: Split.Aux[OUT2, PL, P, HNil]): KvpGroup[OUT2, OUT2L] =
      KvpGroupHead[OUT2, OUT2L, P, PL, HNil, Nat._0](kvp,
                                                     KvpNil,
                                                     prepend,
                                                     split,
                                                     List.empty)

    override def ::[H](v: KeyValueDefinition[H])
      : KvpSingleValueHead[H, HNil, Nat._0, H :: HNil] =
      KvpSingleValueHead(v, List.empty, this)

  }

  /** This allows the XMapData to be attached to a KvpGroup */
  final case class KvpXMapDataHead[A: Manifest,
                                    HT <: HList,
                                    NT <: Nat,
                                    HO <: A :: HT,
                                    XL <:HList,
                                    XLL <: Nat](xmapData: XMapData[XL,XLL,A],
                                                validations: List[ValidationOp[HO]],
                                                tail: KvpGroup[HT, NT]
  ) extends KvpGroup[HO, Succ[NT]] {

    val manifestOfA: Manifest[A] = manifest[A]

    override def :::[HO2 <: HList, NO2 <: Nat, HP <: HList, NP <: Nat](
        kvp: KvpGroup[HP, NP])(
        implicit prepend: Prepend.Aux[HP, HO, HO2],
        lengthP: Length.Aux[HP, NP],
        length: Length.Aux[HO2, NO2],
        split: Split.Aux[HO2, NP, HP, HO]): KvpGroup[HO2, NO2] =
      KvpGroupHead[HO2,NO2,HP,NP,HO,Succ[NT]](kvp, this, prepend, split, List.empty)

    override def ::[H](v: KeyValueDefinition[H])
      : KvpSingleValueHead[H, HO, Succ[NT], H :: HO] =
      KvpSingleValueHead(v, List.empty, this)
  }

  final case class KvpSingleValueHead[H, T <: HList, TL <: Nat, OUT <: H :: T](
      fieldDefinition: KeyValueDefinition[H],
      validations: List[ValidationOp[OUT]],
      tail: KvpGroup[T, TL]
  ) extends KvpGroup[OUT, Succ[TL]] {

    /**
      *
      * When we combine groups, we want to keep the validations separate, but we want to combine the result.
      *
      * @param kvp The Group to append to this KvpGroup
      * @tparam HO2 New HList which combines L (from this) and P (from others)
      * @tparam P The HList output type of kvp
      */
    override def :::[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
        kvp: KvpGroup[P, PL])(
        implicit prepend: hlist.Prepend.Aux[P, OUT, HO2],
        lengthP: Length.Aux[P, PL],
        length: Length.Aux[HO2, NO2],
        split: Split.Aux[HO2, PL, P, OUT]): KvpGroup[HO2, NO2] =
      KvpGroupHead[HO2, NO2, P, PL, OUT, Succ[TL]](kvp,
                                                      this,
                                                      prepend,
                                                      split,
                                                      List.empty)

    override def ::[A](v: KeyValueDefinition[A])
      : KvpSingleValueHead[A, OUT, Succ[TL], A :: OUT] =
      KvpSingleValueHead[A, OUT, Succ[TL], A :: OUT](v, List.empty, this)

    def validate(v: ValidationOp[OUT]): KvpSingleValueHead[H, T, TL, OUT] =
      this.copy(validations = v :: validations)
  }

  /** This is a group of KvpGroup that are grouped and the validations match the entire group.  */
  final case class KvpGroupHead[HO <: HList,
                                NO <: Nat,
                                H <: HList,
                                HL <: Nat,
                                T <: HList,
                                TL <: Nat](
      head: KvpGroup[H, HL],
      tail: KvpGroup[T, TL],
      prepend: Prepend.Aux[H, T, HO],
      split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
      validations: List[ValidationOp[HO]]
  ) extends KvpGroup[HO, NO] {

    /**
      *
      * When we combine groups, we want to keep the validations separete, but we want to combine the result.
      *
      * @param kvp The KvpGroup to append to this group.
      * @tparam HO2 New HList which combines L (from this) and P (from others)
      * @tparam P The HList output type of the kvp group we are appending.
      */
    override def :::[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
        kvp: KvpGroup[P, PL])(
        implicit prepend: Prepend.Aux[P, HO, HO2],
        lengthP: Length.Aux[P, PL],
        length: Length.Aux[HO2, NO2],
        split: Split.Aux[HO2, PL, P, HO]
    ): KvpGroup[HO2, NO2] =
      KvpGroupHead[HO2, NO2, P, PL, HO, NO](kvp,
                                                  this,
                                                  prepend,
                                                  split,
                                                  List.empty)

    override def ::[P](kvd: KeyValueDefinition[P])
      : KvpSingleValueHead[P, HO, NO, P :: HO] =
      KvpSingleValueHead[P, HO, NO, P :: HO](kvd, List.empty, this)

    def validate(v: ValidationOp[HO]): KvpGroupHead[HO, NO, H, HL, T, TL] =
      this.copy(validations = v :: validations)

  }

}
