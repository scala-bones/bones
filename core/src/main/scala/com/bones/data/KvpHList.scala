package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{::, Generic, HList, HNil, Nat, Succ}
import shapeless.ops.hlist
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Length, Prepend, Split, Tupler}

/**
  * Base trait of a ValueDefinition where the value is a list of data.
  * HList stands for Heterogeneous List.
  *
  * @tparam H The HList this value represents.
  * @tparam N The length of this HList
  */
sealed abstract class KvpHList[ALG[_], H <: HList, N <: Nat] {

  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, H]): HListConvert[ALG, H, N, A] =
    HListConvert(this, gen.from, gen.to, validation.toList)

  def convert[A: Manifest](implicit gen: Generic.Aux[A, H]): HListConvert[ALG, H, N, A] =
    convert[A]()

  def tupled[Tup <: Product: Manifest](
    implicit tupler: Tupler.Aux[H, Tup],
    gen: Generic[Tup]
  ): HListConvert[ALG, H, N, Tup] = tupled[Tup]()

  def tupled[Tup <: Product: Manifest](tupledValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[H, Tup],
    gen: Generic[Tup]
  ): HListConvert[ALG, H, N, Tup] =
    HListConvert[ALG, H, N, Tup](
      this,
      (h: H) => tupler.apply(h),
      (t: Tup) => {
        val out = gen.to(t).asInstanceOf[H]
        out
      },
      tupledValidations.toList)

  def xmap[A: Manifest](
    f: H => A,
    g: A => H,
    validations: ValidationOp[A]*): HListConvert[ALG, H, N, A] =
    HListConvert(this, f, g, validations.toList)

  def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHList[ALG, HP, NP])(
    implicit prepend: Prepend.Aux[HP, H, HO],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO, NO],
    split: Split.Aux[HO, NP, HP, H]
  ): KvpHList[ALG, HO, NO] = prependHList(kvp)

  def prependSchema[A: Manifest](schema: BonesSchema[ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpHList[ALG, A :: H, Succ[N]] =
    KvpConcreteTypeHead[ALG, A, H, N](schema, List.empty, this, isHCons)

  def prependHList[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHList[ALG, HP, NP])(
    implicit prepend: Prepend.Aux[HP, H, HO],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO, NO],
    split: Split.Aux[HO, NP, HP, H]
  ): KvpHList[ALG, HO, NO]

  def prependSingleValue[A](v: KeyValueDefinition[ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpSingleValueHead[ALG, A, H, N, A :: H]

  def >>:[A](v: KeyValueDefinition[ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpSingleValueHead[ALG, A, H, N, A :: H] =
    prependSingleValue(v)

  /**
    * Use this operator when you want to prefix a custom algebra.
    * @param input The Key, Value Pair to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def ::[A](input: (String, ALG[A]))(
    implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpSingleValueHead[ALG, A, H, N, A :: H] =
    prependSingleValue(KeyValueDefinition(input._1, Right(input._2), None, None))(isHCons)

  /**
    * Use this operator when you want to prefix a custom algebra with a description
    * @param input The Key, Value Pair and Description, Example to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def ::[A](input: (String, ALG[A], String, A))(
    implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpSingleValueHead[ALG, A, H, N, A :: H] =
    prependSingleValue(
      KeyValueDefinition(input._1, Right(input._2), Some(input._3), Some(input._4)))(isHCons)

  /**
    * Prefixes one of the Algebra types to this HLIst
    * @param input The Key, Value Pair to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def :<:[A](input: (String, KvpCollection[ALG, A]))(
    implicit isHCons: Aux[A :: H, A, H]): KvpSingleValueHead[ALG, A, H, N, A :: H] =
    prependSingleValue(new KeyValueDefinition(input._1, Left(input._2), None, None))(isHCons)

  def :<:[A](input: (String, KvpCollection[ALG, A], String, A))(
    implicit isHCons: Aux[A :: H, A, H]): KvpSingleValueHead[ALG, A, H, N, A :: H] =
    prependSingleValue(
      new KeyValueDefinition(input._1, Left(input._2), Some(input._3), Some(input._4)))(isHCons)

  def :><:[OUT2 <: HList, OUT2L <: Nat, A: Manifest, HX <: HList, NX <: Nat](
    dc: BonesSchema[ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpConcreteTypeHead[ALG, A, H, N] =
    KvpConcreteTypeHead[ALG, A, H, N](dc, List.empty, this, isHCons)

}

/** The Nil as in an empty HList.
  */
case class KvpNil[ALG[_]]() extends KvpHList[ALG, HNil, Nat._0] {

  override def prependHList[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](
    kvp: KvpHList[ALG, P, PL])(
    implicit prepend: hlist.Prepend.Aux[P, HNil, OUT2],
    lengthP: Length.Aux[P, PL],
    length: Length.Aux[OUT2, OUT2L],
    split: Split.Aux[OUT2, PL, P, HNil]): KvpHList[ALG, OUT2, OUT2L] =
    KvpHListHead[ALG, OUT2, OUT2L, P, PL, HNil, Nat._0](kvp, this, prepend, split, List.empty)

  override def prependSingleValue[H](v: KeyValueDefinition[ALG, H])(
    implicit isHCons: IsHCons.Aux[H :: HNil, H, HNil])
    : KvpSingleValueHead[ALG, H, HNil, Nat._0, H :: HNil] =
    KvpSingleValueHead(v, List.empty, this, isHCons)

}

/**
  * This allows the HListConvert to be attached to a KvpHList.  For example,
  * at a type level, we can combine a Case class and a generic type.
  * @param validations The list of validations tied to the entire data structure.
  * @param tail The tail of the HList which the A is prepended to.
  * @param isHCons Provides the ability to join A :: HT and Split HO
  * @tparam A The concrete class A which is at the head of the data structure.
  * @tparam HT HList Tail
  * @tparam NT Nat length of tail
  */
final case class KvpConcreteTypeHead[ALG[_], A: Manifest, HT <: HList, NT <: Nat](
  bonesSchema: BonesSchema[ALG, A],
  validations: List[ValidationOp[A :: HT]],
  tail: KvpHList[ALG, HT, NT],
  isHCons: IsHCons.Aux[A :: HT, A, HT])
    extends KvpHList[ALG, A :: HT, Succ[NT]] {

  val manifestOfA: Manifest[A] = manifest[A]

  override def prependHList[HO2 <: HList, NO2 <: Nat, HP <: HList, NP <: Nat](
    kvp: KvpHList[ALG, HP, NP])(
    implicit prepend: Prepend.Aux[HP, A :: HT, HO2],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO2, NO2],
    split: Split.Aux[HO2, NP, HP, A :: HT]): KvpHList[ALG, HO2, NO2] =
    KvpHListHead[ALG, HO2, NO2, HP, NP, A :: HT, Succ[NT]](kvp, this, prepend, split, List.empty)

  override def prependSingleValue[B](v: KeyValueDefinition[ALG, B])(
    implicit isHCons: Aux[B :: A :: HT, B, A :: HT])
    : KvpSingleValueHead[ALG, B, A :: HT, Succ[NT], B :: A :: HT] =
    KvpSingleValueHead(v, List.empty, this, isHCons)

}

/** The head of the HList has a known KeyValueDefinition. */
final case class KvpSingleValueHead[ALG[_], H, T <: HList, TL <: Nat, OUT <: H :: T](
  fieldDefinition: KeyValueDefinition[ALG, H],
  validations: List[ValidationOp[OUT]],
  tail: KvpHList[ALG, T, TL],
  isHCons: IsHCons.Aux[OUT, H, T]
) extends KvpHList[ALG, OUT, Succ[TL]] {

  /**
    *
    * When we combine groups, we want to keep the validations separate, but we want to combine the result.
    *
    * @param kvp The HList to append to this KvpHList
    * @tparam HO2 New HList which combines L (from this) and P (from others)
    * @tparam P   The HList output type of kvp
    */
  override def prependHList[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
    kvp: KvpHList[ALG, P, PL])(
    implicit prepend: hlist.Prepend.Aux[P, OUT, HO2],
    lengthP: Length.Aux[P, PL],
    length: Length.Aux[HO2, NO2],
    split: Split.Aux[HO2, PL, P, OUT]): KvpHList[ALG, HO2, NO2] =
    KvpHListHead[ALG, HO2, NO2, P, PL, OUT, Succ[TL]](kvp, this, prepend, split, List.empty)

  override def prependSingleValue[A](v: KeyValueDefinition[ALG, A])(
    implicit isHCons: Aux[A :: OUT, A, OUT]): KvpSingleValueHead[ALG, A, OUT, Succ[TL], A :: OUT] =
    KvpSingleValueHead[ALG, A, OUT, Succ[TL], A :: OUT](v, List.empty, this, isHCons)

  def validate(v: ValidationOp[OUT]): KvpSingleValueHead[ALG, H, T, TL, OUT] =
    this.copy(validations = v :: validations)
}

/** This is a group of KvpHList that are grouped and the validations match the entire group.  */
final case class KvpHListHead[
  ALG[_],
  HO <: HList,
  NO <: Nat,
  H <: HList,
  HL <: Nat,
  T <: HList,
  TL <: Nat](
  head: KvpHList[ALG, H, HL],
  tail: KvpHList[ALG, T, TL],
  prepend: Prepend.Aux[H, T, HO],
  split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
  validations: List[ValidationOp[HO]]
) extends KvpHList[ALG, HO, NO] {

  /**
    *
    * When we combine groups, we want to keep the validations separate, but we want to combine the result.
    *
    * @param kvp The KvpHList to append to this group.
    * @tparam HO2 New HList which combines L (from this) and P (from others)
    * @tparam P   The HList output type of the kvp group we are appending.
    */
  override def prependHList[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
    kvp: KvpHList[ALG, P, PL])(
    implicit prepend: Prepend.Aux[P, HO, HO2],
    lengthP: Length.Aux[P, PL],
    length: Length.Aux[HO2, NO2],
    split: Split.Aux[HO2, PL, P, HO]
  ): KvpHListHead[ALG, HO2, NO2, P, PL, HO, NO] =
    KvpHListHead[ALG, HO2, NO2, P, PL, HO, NO](kvp, this, prepend, split, List.empty)

  override def prependSingleValue[A](kvd: KeyValueDefinition[ALG, A])(
    implicit isHCons: Aux[A :: HO, A, HO]): KvpSingleValueHead[ALG, A, HO, NO, A :: HO] =
    KvpSingleValueHead[ALG, A, HO, NO, A :: HO](kvd, List.empty, this, isHCons)

  def validate(v: ValidationOp[HO]): KvpHListHead[ALG, HO, NO, H, HL, T, TL] =
    this.copy(validations = v :: validations)

}
