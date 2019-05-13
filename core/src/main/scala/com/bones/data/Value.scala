package com.bones.data

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.free.FreeApplicative
import com.bones.data.Error.CanNotConvert
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Length, Prepend, Split}
import shapeless.{::, Generic, HList, HNil, Nat, Succ}

/** The Value in Key-Value Pair */
object Value {

  /** KvpValue is meant to be the 'value' of a key value pair.
    * This can be one of the pre-defined primitive 'Bones' types or a product type,
    * one of KvpHList, SumType or
    * */
  sealed abstract class KvpValue[A: Manifest] {

    val manifestOfA: Manifest[A] = manifest[A]

    def asSumType[B: Manifest](description: String,
                               fab: (A, Value.Path) => Either[CanNotConvert[A, B], B],
                               fba: B => A,
                               keys: List[A],
                               validations: List[ValidationOp[B]]
                              ): SumTypeData[A, B] = {
      SumTypeData[A, B](this, fab, fba, keys, validations)
    }
  }

  type ValueDefinition[A] = FreeApplicative[KvpValue, A]

  /** Wraps a data definition to mark the field optional */
  case class OptionalKvpValueDefinition[B: Manifest](valueDefinitionOp: KvpValue[B])
    extends KvpValue[Option[B]] {}

  /** Syntactic sugar to wrap the data definition to allow 'optional' syntax on a KvpValue. */
  trait ToOptionalData[B] {
    self: KvpValue[B] =>

    private implicit val manifestOfB: Manifest[B] = self.manifestOfA
    val optional: OptionalKvpValueDefinition[B] = OptionalKvpValueDefinition[B](self)
  }

  /** Syntactic sugar to wrap the definition in a List type. */
  trait ToListData[B] {
    self: KvpValue[B] =>
    private implicit val manifestOfB: Manifest[B] = self.manifestOfA
    val list: ListData[B] = ListData[B](self, List.empty)
  }

  final case class BooleanData(validations: List[ValidationOp[Boolean]])
    extends KvpValue[Boolean]
      with ToOptionalData[Boolean]

  final case class EitherData[A: Manifest, B: Manifest](definitionA: KvpValue[A],
                                                        definitionB: KvpValue[B])
    extends KvpValue[Either[A, B]]
      with ToOptionalData[Either[A, B]] {}

  final case class IntData(validations: List[ValidationOp[Int]])
    extends KvpValue[Int]
      with ToOptionalData[Int]

  final case class LongData(validations: List[ValidationOp[Long]])
    extends KvpValue[Long]
      with ToOptionalData[Long]

  final case class ListData[T: Manifest](tDefinition: KvpValue[T],
                                         validations: List[ValidationOp[List[T]]])
    extends KvpValue[List[T]]
      with ToOptionalData[List[T]]

  final case class StringData(validations: List[ValidationOp[String]])
    extends KvpValue[String]
      with ToOptionalData[String]

  final case class FloatData(validations: List[ValidationOp[Float]])
    extends KvpValue[Float]
      with ToOptionalData[Float]

  final case class DoubleData(validations: List[ValidationOp[Double]])
    extends KvpValue[Double]
      with ToOptionalData[Double]

  final case class BigDecimalData(validations: List[ValidationOp[BigDecimal]])
    extends KvpValue[BigDecimal]
      with ToOptionalData[BigDecimal]

  // base64-encoded characters, for example, U3dhZ2dlciByb2Nrcw==
  final case class ByteArrayData(validations: List[KvpValue[Array[Byte]]])
    extends KvpValue[Array[Byte]]
      with ToOptionalData[Array[Byte]]

  final case class DateTimeData(dateFormat: DateTimeFormatter,
                                formatDescription: String,
                                validations: List[ValidationOp[ZonedDateTime]])
    extends KvpValue[ZonedDateTime]
      with ToOptionalData[ZonedDateTime]

  final case class UuidData(validations: List[ValidationOp[UUID]])
    extends KvpValue[UUID]
      with ToOptionalData[UUID]

  final case class EnumerationStringData[A: Manifest](enumeration: Enumeration,
                                                      validations: List[ValidationOp[A]])
    extends KvpValue[A]
      with ToOptionalData[A] {}

  final case class KvpHListValue[H <: HList : Manifest, HL <: Nat](kvpHList: KvpHList[H, HL],
                                                                   validations: List[ValidationOp[H]])
    extends KvpValue[H]
      with ToOptionalData[H] {

    def convert[Z: Manifest](validation: ValidationOp[Z]*)(
      implicit gen: Generic.Aux[Z, H]): HListConvert[H, HL, Z] =
      HListConvert(kvpHList, gen.from, gen.to, validation.toList)
  }

  trait BonesSchema[A] {
    val manifestOfA: Manifest[A]
  }

  final case class HListConvert[A <: HList, AL <: Nat, B: Manifest](from: KvpHList[A, AL],
                                                                    fab: A => B,
                                                                    fba: B => A,
                                                                    validations: List[ValidationOp[B]]
                                                                   )
    extends KvpValue[B]
      with ToOptionalData[B]
      with ToListData[B]
      with BonesSchema[B] {}

  type Path = List[String]
  final case class SumTypeData[A, B: Manifest](from: KvpValue[A],
                                               fab: (A, Path) => Either[CanNotConvert[A, B], B],
                                               fba: B => A,
                                               keys: List[A],
                                               validations: List[ValidationOp[B]]
                                              ) extends KvpValue[B] {}

  /**
    * Base trait of a ValueDefinition where the value is a list of data.
    * HList stands for Heterogeneous List.
    *
    * @tparam H The HList this value represents.
    * @tparam N The length of this HList
    */
  sealed abstract class KvpHList[H <: HList, N <: Nat] {

    def convert[A: Manifest](validation: ValidationOp[A]*)(
      implicit gen: Generic.Aux[A, H]): HListConvert[H, N, A] =
      HListConvert(this, gen.from, gen.to, validation.toList)

    def convert[A: Manifest](implicit gen: Generic.Aux[A, H]): HListConvert[H, N, A] = convert[A]()

    def xmap[A: Manifest](f: H => A, g: A => H, validations: ValidationOp[A]*) =
      HListConvert(this, f, g, validations.toList)

    def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHList[HP, NP])(
      implicit prepend: Prepend.Aux[HP, H, HO],
      lengthP: Length.Aux[HP, NP],
      length: Length.Aux[HO, NO],
      split: Split.Aux[HO, NP, HP, H]
    ): KvpHList[HO, NO]

    def ::[A](v: KeyValueDefinition[A])(implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpSingleValueHead[A, H, N, A :: H]

    /* The ability to prefix an HListConvert (case class) to a KvpHList */
    def :><:[OUT2 <: HList, OUT2L <: Nat, A: Manifest, HX <: HList, NX <: Nat](
                                                                                dc: HListConvert[HX, NX, A]): KvpConcreteTypeHead[A, H, N, A :: H, HX, NX] =
      KvpConcreteTypeHead[A, H, N, A :: H, HX, NX](dc, List.empty, this)

  }

  /** The Nil as in an empty HList.
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


    override def ::[H](v: KeyValueDefinition[H])(implicit isHCons: IsHCons.Aux[H :: HNil, H, HNil])
    : KvpSingleValueHead[H, HNil, Nat._0, H :: HNil] =
      KvpSingleValueHead(v, List.empty, this, isHCons)

  }

  /** This allows the HListConvert to be attached to a KvpHList */
  final case class KvpConcreteTypeHead[A: Manifest,
  HT <: HList,
  NT <: Nat,
  HO <: A :: HT,
  XL <: HList,
  XLL <: Nat](
               hListConvert: HListConvert[XL, XLL, A],
               validations: List[ValidationOp[HO]],
               tail: KvpHList[HT, NT])
    extends KvpHList[HO, Succ[NT]] {

    val manifestOfA: Manifest[A] = manifest[A]

    override def :::[HO2 <: HList, NO2 <: Nat, HP <: HList, NP <: Nat](
                                                                        kvp: KvpHList[HP, NP])(
                                                                        implicit prepend: Prepend.Aux[HP, HO, HO2],
                                                                        lengthP: Length.Aux[HP, NP],
                                                                        length: Length.Aux[HO2, NO2],
                                                                        split: Split.Aux[HO2, NP, HP, HO]): KvpHList[HO2, NO2] =
      KvpHListHead[HO2, NO2, HP, NP, HO, Succ[NT]](kvp,
        this,
        prepend,
        split,
        List.empty)


    override def ::[B](v: KeyValueDefinition[B])(implicit isHCons: Aux[B :: HO, B, HO]):
    KvpSingleValueHead[B, HO, Succ[NT], B :: HO] = KvpSingleValueHead(v, List.empty, this, isHCons)

  }

  /** The head of the HList has a known KeyValueDefinition. */
  final case class KvpSingleValueHead[H, T <: HList, TL <: Nat, OUT <: H :: T](
                                                                                fieldDefinition: KeyValueDefinition[H],
                                                                                validations: List[ValidationOp[OUT]],
                                                                                tail: KvpHList[T, TL],
                                                                                isHCons: IsHCons.Aux[OUT, H, T]
                                                                              ) extends KvpHList[OUT, Succ[TL]] {

    /**
      *
      * When we combine groups, we want to keep the validations separate, but we want to combine the result.
      *
      * @param kvp The HList to append to this KvpHList
      * @tparam HO2 New HList which combines L (from this) and P (from others)
      * @tparam P   The HList output type of kvp
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


    override def ::[A](v: KeyValueDefinition[A])(implicit isHCons: Aux[A :: OUT, A, OUT]):
    KvpSingleValueHead[A, OUT, Succ[TL], A :: OUT] = KvpSingleValueHead[A, OUT, Succ[TL], A :: OUT](v, List.empty, this, isHCons)

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
      * @tparam P   The HList output type of the kvp group we are appending.
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

    override def ::[A](kvd: KeyValueDefinition[A])(implicit isHCons: Aux[A :: HO, A, HO]):
    KvpSingleValueHead[A, HO, NO, A :: HO] = KvpSingleValueHead[A, HO, NO, A :: HO](kvd, List.empty, this, isHCons)


    def validate(v: ValidationOp[HO]): KvpHListHead[HO, NO, H, HL, T, TL] =
      this.copy(validations = v :: validations)

  }

}
