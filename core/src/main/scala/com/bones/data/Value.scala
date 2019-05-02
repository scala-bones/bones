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
    }
  }

  /** ValueDefinitionOp is meant to be the 'value' of a key value pair.
    * This can be one of the pre-defined primitive 'Bones' types or a product type,
    * one of KvpHList, SumType or
    * */
  sealed abstract class ValueDefinitionOp[A:Manifest] {

    val manifestOfA = manifest[A]

    def asSumType[B:Manifest](
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
  case class OptionalValueDefinition[B:Manifest](valueDefinitionOp: ValueDefinitionOp[B])
      extends ValueDefinitionOp[Option[B]] {}

  /** Syntactic sugar to wrap the data definition in an Optional type. */
  trait ToOptionalData[B] { self: ValueDefinitionOp[B] =>

    private implicit val manifestOfB = self.manifestOfA
    val optional: OptionalValueDefinition[B] = OptionalValueDefinition[B](self)
  }

  /** Syntactic sugar to wrap the definition in a List type. */
  trait ToListData[B] { self: ValueDefinitionOp[B] =>
    private implicit val manifestOfB = self.manifestOfA
    val list: ListData[B] = ListData[B](self, List.empty)
  }

  final case class BooleanData(validations: List[ValidationOp[Boolean]])
      extends ValueDefinitionOp[Boolean]
      with ToOptionalData[Boolean]
  final case class EitherData[A:Manifest, B:Manifest](definitionA: ValueDefinitionOp[A],
                                    definitionB: ValueDefinitionOp[B])
      extends ValueDefinitionOp[Either[A, B]]
      with ToOptionalData[Either[A, B]] {}
  final case class LongData(validations: List[ValidationOp[Long]])
      extends ValueDefinitionOp[Long]
      with ToOptionalData[Long]
  final case class ListData[T:Manifest](tDefinition: ValueDefinitionOp[T],
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
  }

  final case class KvpHListValue[H <: HList : Manifest, HL <: Nat](
                                                                   kvpHList: KvpHList[H, HL],
                                                                   validations: List[ValidationOp[H]])
      extends ValueDefinitionOp[H]
      with ToOptionalData[H] {

    def convert[Z: Manifest](validation: ValidationOp[Z]*)(
        implicit gen: Generic.Aux[Z, H]): XMapData[H, HL, Z] =
      XMapData(kvpHList, gen.from, gen.to, validation.toList)
  }

  trait BonesSchema[A] {
    val manifestOfA: Manifest[A]
  }
  final case class XMapData[A <: HList, AL <: Nat, B: Manifest](
                                                                 from: KvpHList[A, AL],
                                                                 fab: A => B,
                                                                 fba: B => A,
                                                                 validations: List[ValidationOp[B]])
    extends ValueDefinitionOp[B] with ToOptionalData[B] with ToListData[B] with BonesSchema[B]{
  }

  final case class SumTypeData[A, B:Manifest](
      from: ValueDefinitionOp[A],
      fab: (A, List[String]) => Either[CanNotConvert[A, B], B],
      fba: B => A,
      keys: List[A],
      validations: List[ValidationOp[B]]
  ) extends ValueDefinitionOp[B] {}

  /**
    * Base trait of a ValueDefinition where the value is a list of data.
    * HList stands for Heterogeneous List.
    * @tparam H The HList this value represents.
    * @tparam N The length of this HList
    */
  sealed trait KvpHList[H <: HList, N <: Nat] {

    /**
      *
      * @param validation
      * @param gen
      * @tparam A
      * @return
      */
    def convert[A: Manifest](validation: ValidationOp[A]*)(
        implicit gen: Generic.Aux[A, H]): XMapData[H, N, A] =
      XMapData(this, gen.from, gen.to, validation.toList)

    def convert[A: Manifest](
        implicit gen: Generic.Aux[A, H]): XMapData[H, N, A] = convert[A]()

    def xmap[A: Manifest](f: H => A, g: A => H, validations: ValidationOp[A]*) =
      XMapData(this, f, g, validations.toList)

    def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](
        kvp: KvpHList[HP, NP])(
        implicit prepend: Prepend.Aux[HP, H, HO],
        lengthP: Length.Aux[HP, NP],
        length: Length.Aux[HO, NO],
        split: Split.Aux[HO, NP, HP, H]
    ): KvpHList[HO, NO]

    def ::[A](v: KeyValueDefinition[A]): KvpSingleValueHead[A, H, N, A :: H]

    /* The ability to prefix an XMapData (case class) to a KvpHList */
    def :><:[OUT2 <: HList, OUT2L <: Nat, A: Manifest,HX<:HList, NX<:Nat](dc: XMapData[HX, NX, A]):
    KvpXMapDataHead[A, H, N, A :: H,HX,NX] =
      KvpXMapDataHead[A,H,N,A::H,HX,NX](dc, List.empty, this)

  }


  /**
    */
  object KvpNil extends KvpHList[HNil, Nat._0] {

    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](
        kvp: KvpHList[P, PL])(
        implicit prepend: hlist.Prepend.Aux[P, HNil, OUT2],
        lengthP: Length.Aux[P, PL],
        length: Length.Aux[OUT2, OUT2L],
        split: Split.Aux[OUT2, PL, P, HNil]): KvpHList[OUT2, OUT2L] =
      KvpHListHead[OUT2, OUT2L, P, PL, HNil, Nat._0](kvp,
                                                     KvpNil,
                                                     prepend,
                                                     split,
                                                     List.empty)

    override def ::[H](v: KeyValueDefinition[H])
      : KvpSingleValueHead[H, HNil, Nat._0, H :: HNil] =
      KvpSingleValueHead(v, List.empty, this)

  }

  /** This allows the XMapData to be attached to a KvpHList */
  final case class KvpXMapDataHead[A: Manifest,
                                    HT <: HList,
                                    NT <: Nat,
                                    HO <: A :: HT,
                                    XL <:HList,
                                    XLL <: Nat](xmapData: XMapData[XL,XLL,A],
                                                validations: List[ValidationOp[HO]],
                                                tail: KvpHList[HT, NT]
  ) extends KvpHList[HO, Succ[NT]] {

    val manifestOfA: Manifest[A] = manifest[A]

    override def :::[HO2 <: HList, NO2 <: Nat, HP <: HList, NP <: Nat](
        kvp: KvpHList[HP, NP])(
        implicit prepend: Prepend.Aux[HP, HO, HO2],
        lengthP: Length.Aux[HP, NP],
        length: Length.Aux[HO2, NO2],
        split: Split.Aux[HO2, NP, HP, HO]): KvpHList[HO2, NO2] =
      KvpHListHead[HO2,NO2,HP,NP,HO,Succ[NT]](kvp, this, prepend, split, List.empty)

    override def ::[H](v: KeyValueDefinition[H])
      : KvpSingleValueHead[H, HO, Succ[NT], H :: HO] =
      KvpSingleValueHead(v, List.empty, this)
  }

  final case class KvpSingleValueHead[H, T <: HList, TL <: Nat, OUT <: H :: T](
      fieldDefinition: KeyValueDefinition[H],
      validations: List[ValidationOp[OUT]],
      tail: KvpHList[T, TL]
  ) extends KvpHList[OUT, Succ[TL]] {

    /**
      *
      * When we combine groups, we want to keep the validations separate, but we want to combine the result.
      *
      * @param kvp The HList to append to this KvpHList
      * @tparam HO2 New HList which combines L (from this) and P (from others)
      * @tparam P The HList output type of kvp
      */
    override def :::[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
        kvp: KvpHList[P, PL])(
        implicit prepend: hlist.Prepend.Aux[P, OUT, HO2],
        lengthP: Length.Aux[P, PL],
        length: Length.Aux[HO2, NO2],
        split: Split.Aux[HO2, PL, P, OUT]): KvpHList[HO2, NO2] =
      KvpHListHead[HO2, NO2, P, PL, OUT, Succ[TL]](kvp,
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

  /** This is a group of KvpHList that are grouped and the validations match the entire group.  */
  final case class KvpHListHead[HO <: HList,
                                NO <: Nat,
                                H <: HList,
                                HL <: Nat,
                                T <: HList,
                                TL <: Nat](
                                            head: KvpHList[H, HL],
                                            tail: KvpHList[T, TL],
                                            prepend: Prepend.Aux[H, T, HO],
                                            split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
                                            validations: List[ValidationOp[HO]]
  ) extends KvpHList[HO, NO] {

    /**
      *
      * When we combine groups, we want to keep the validations separete, but we want to combine the result.
      *
      * @param kvp The KvpHList to append to this group.
      * @tparam HO2 New HList which combines L (from this) and P (from others)
      * @tparam P The HList output type of the kvp group we are appending.
      */
    override def :::[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
        kvp: KvpHList[P, PL])(
        implicit prepend: Prepend.Aux[P, HO, HO2],
        lengthP: Length.Aux[P, PL],
        length: Length.Aux[HO2, NO2],
        split: Split.Aux[HO2, PL, P, HO]
    ): KvpHList[HO2, NO2] =
      KvpHListHead[HO2, NO2, P, PL, HO, NO](kvp,
                                                  this,
                                                  prepend,
                                                  split,
                                                  List.empty)

    override def ::[P](kvd: KeyValueDefinition[P])
      : KvpSingleValueHead[P, HO, NO, P :: HO] =
      KvpSingleValueHead[P, HO, NO, P :: HO](kvd, List.empty, this)

    def validate(v: ValidationOp[HO]): KvpHListHead[HO, NO, H, HL, T, TL] =
      this.copy(validations = v :: validations)

  }

}
