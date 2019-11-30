package com.bones.coproduct

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{::, Coproduct, Generic, HList, HNil, Nat, Succ}
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
sealed abstract class KvpHList[H <: HList, N <: Nat, COV[_]] {

  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, H]): HListConvert[H, N, A, COV] =
    HListConvert(this, gen.from, gen.to, validation.toList)

  def convert[A: Manifest](implicit gen: Generic.Aux[A, H]): HListConvert[H, N, A, COV] = convert[A]()

  def tupled[Tup<:Product:Manifest](
                                     implicit tupler: Tupler.Aux[H,Tup],
                                     gen: Generic[Tup]
                                   ): HListConvert[H,N,Tup,COV] = tupled[Tup]()

  def tupled[Tup<:Product:Manifest](tupledValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[H,Tup],
    gen: Generic[Tup]
  ): HListConvert[H,N,Tup,COV] =
    HListConvert[H,N,Tup,COV](this, (h: H) => tupler.apply(h), (t: Tup) => gen.to(t).asInstanceOf[H], tupledValidations.toList)


  def xmap[A: Manifest](f: H => A, g: A => H, validations: ValidationOp[A]*) =
    HListConvert(this, f, g, validations.toList)

  def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHList[HP, NP, COV])(
    implicit prepend: Prepend.Aux[HP, H, HO],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO, NO],
    split: Split.Aux[HO, NP, HP, H]
  ): KvpHList[HO, NO, COV] = prependHList(kvp)

  def prependHList[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHList[HP, NP, COV])(
    implicit prepend: Prepend.Aux[HP, H, HO],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO, NO],
    split: Split.Aux[HO, NP, HP, H]
  ): KvpHList[HO, NO, COV]

  def prependSingleValue[A](v: KeyValueDefinition[A,COV])(implicit isHCons: IsHCons.Aux[A :: H, A, H]):
    KvpSingleValueHead[A, H, N, A :: H, COV]


  def ::[A](v: KeyValueDefinition[A,COV])(implicit isHCons: IsHCons.Aux[A :: H, A, H]):
    KvpSingleValueHead[A, H, N, A :: H, COV] = prependSingleValue(v)

  /**
    * Use this operator when you want to prefix a custom algebra.
    * @param input
    * @param isHCons
    * @tparam A
    * @return
    */
  def :^:[A](input: (String, COV[A]))(implicit isHCons: IsHCons.Aux[A :: H, A, H]):
    KvpSingleValueHead[A, H, N, A :: H, COV] =
      prependSingleValue(new KeyValueDefinition(input._1, Right(input._2)))(isHCons)

  /**
    *
    * @param input
    * @param isHCons
    * @tparam A
    * @return
    */
  def ::[A] ( input: (String, KvpValue[A]) )(implicit isHCons: Aux[A :: H, A, H]):
  KvpSingleValueHead[A, H, N, A :: H, COV] =
    prependSingleValue(new KeyValueDefinition(input._1, Left(input._2)))(isHCons)


  /* The ability to prefix an HListConvert (case class) to a KvpHList */
  def :><:[OUT2 <: HList, OUT2L <: Nat, A: Manifest, HX <: HList, NX <: Nat]
  (dc: HListConvert[HX, NX, A, COV])
  (implicit isHCons: IsHCons.Aux[A :: H, A, H]): KvpConcreteTypeHead[A, H, N, A :: H, HX, NX, COV] =
    KvpConcreteTypeHead[A, H, N, A :: H, HX, NX, COV](dc, List.empty, this, isHCons)






}

/** The Nil as in an empty HList.
  */
case class KvpNil[COV[_]]() extends KvpHList[HNil, Nat._0, COV] {

  override def prependHList[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](
                                                                        kvp: KvpHList[P, PL, COV])(
                                                                        implicit prepend: hlist.Prepend.Aux[P, HNil, OUT2],
                                                                        lengthP: Length.Aux[P, PL],
                                                                        length: Length.Aux[OUT2, OUT2L],
                                                                        split: Split.Aux[OUT2, PL, P, HNil]): KvpHList[OUT2, OUT2L, COV] =
    KvpHListHead[OUT2, OUT2L, P, PL, HNil, Nat._0, COV](kvp,
      this,
      prepend,
      split,
      List.empty)


  override def prependSingleValue[H](v: KeyValueDefinition[H,COV])(implicit isHCons: IsHCons.Aux[H :: HNil, H, HNil])
  : KvpSingleValueHead[H, HNil, Nat._0, H :: HNil, COV] =
    KvpSingleValueHead(v, List.empty, this, isHCons)

}

/**
  * This allows the HListConvert to be attached to a KvpHList.  For example,
  * at a type level, we can combine a Case class and a generic type.
  * @param hListConvert provides functions to and from A/XL
  * @param validations The list of validations tied to the entire data structure.
  * @param tail The tail of the HList which the A is prepended to.
  * @param isHCons Provides the ability to join A :: HT and Split HO
  * @tparam A The concrete class A which is at the head of the data structure.
  * @tparam HT HList Tail
  * @tparam NT Nat length of tail
  * @tparam HO The full HList generic representation this instance represents.
  * @tparam XL HList which can be converted to A using HListConvert functions
  * @tparam XLL The Nat length of the generic type this instance represents.
  */
final case class KvpConcreteTypeHead[A: Manifest,
HT <: HList,
NT <: Nat,
HO <: A :: HT,
XL <: HList,
XLL <: Nat,
COV[_]](
             hListConvert: HListConvert[XL, XLL, A, COV],
             validations: List[ValidationOp[HO]],
             tail: KvpHList[HT, NT, COV],
             isHCons: IsHCons.Aux[HO, A, HT])
  extends KvpHList[HO, Succ[NT], COV] {

  val manifestOfA: Manifest[A] = manifest[A]

  override def prependHList[HO2 <: HList, NO2 <: Nat, HP <: HList, NP <: Nat]
  (kvp: KvpHList[HP, NP, COV])
  (implicit prepend: Prepend.Aux[HP, HO, HO2],
   lengthP: Length.Aux[HP, NP],
   length: Length.Aux[HO2, NO2],
   split: Split.Aux[HO2, NP, HP, HO]
  ): KvpHList[HO2, NO2, COV] =
    KvpHListHead[HO2, NO2, HP, NP, HO, Succ[NT], COV](kvp, this, prepend, split, List.empty)


  override def prependSingleValue[B](v: KeyValueDefinition[B,COV])(implicit isHCons: Aux[B :: HO, B, HO]):
  KvpSingleValueHead[B, HO, Succ[NT], B :: HO, COV] = KvpSingleValueHead(v, List.empty, this, isHCons)

}

/** The head of the HList has a known KeyValueDefinition. */
final case class KvpSingleValueHead[H, T <: HList, TL <: Nat, OUT <: H :: T, COV[_]](
                                                                              fieldDefinition: KeyValueDefinition[H,COV],
                                                                              validations: List[ValidationOp[OUT]],
                                                                              tail: KvpHList[T, TL, COV],
                                                                              isHCons: IsHCons.Aux[OUT, H, T]
                                                                            ) extends KvpHList[OUT, Succ[TL], COV] {

  /**
    *
    * When we combine groups, we want to keep the validations separate, but we want to combine the result.
    *
    * @param kvp The HList to append to this KvpHList
    * @tparam HO2 New HList which combines L (from this) and P (from others)
    * @tparam P   The HList output type of kvp
    */
  override def prependHList[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
                                                                     kvp: KvpHList[P, PL, COV])(
                                                                     implicit prepend: hlist.Prepend.Aux[P, OUT, HO2],
                                                                     lengthP: Length.Aux[P, PL],
                                                                     length: Length.Aux[HO2, NO2],
                                                                     split: Split.Aux[HO2, PL, P, OUT]): KvpHList[HO2, NO2, COV] =
    KvpHListHead[HO2, NO2, P, PL, OUT, Succ[TL], COV](kvp,
      this,
      prepend,
      split,
      List.empty)

  override def prependSingleValue[A](v: KeyValueDefinition[A,COV])(implicit isHCons: Aux[A :: OUT, A, OUT]):
  KvpSingleValueHead[A, OUT, Succ[TL], A :: OUT, COV] = KvpSingleValueHead[A, OUT, Succ[TL], A :: OUT, COV](v, List.empty, this, isHCons)

  def validate(v: ValidationOp[OUT]): KvpSingleValueHead[H, T, TL, OUT, COV] =
    this.copy(validations = v :: validations)
}

/** This is a group of KvpHList that are grouped and the validations match the entire group.  */
final case class KvpHListHead[HO <: HList,
NO <: Nat,
H <: HList,
HL <: Nat,
T <: HList,
TL <: Nat,
COV[_]](
            head: KvpHList[H, HL, COV],
            tail: KvpHList[T, TL, COV],
            prepend: Prepend.Aux[H, T, HO],
            split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
            validations: List[ValidationOp[HO]]
          ) extends KvpHList[HO, NO, COV] {

  /**
    *
    * When we combine groups, we want to keep the validations separate, but we want to combine the result.
    *
    * @param kvp The KvpHList to append to this group.
    * @tparam HO2 New HList which combines L (from this) and P (from others)
    * @tparam P   The HList output type of the kvp group we are appending.
    */
  override def prependHList[HO2 <: HList, NO2 <: Nat, P <: HList, PL <: Nat](
                                                                     kvp: KvpHList[P, PL, COV])(
                                                                     implicit prepend: Prepend.Aux[P, HO, HO2],
                                                                     lengthP: Length.Aux[P, PL],
                                                                     length: Length.Aux[HO2, NO2],
                                                                     split: Split.Aux[HO2, PL, P, HO]
                                                                   ): KvpHListHead[HO2, NO2, P, PL, HO, NO, COV] =
    KvpHListHead[HO2, NO2, P, PL, HO, NO, COV](kvp,
      this,
      prepend,
      split,
      List.empty)

  override def prependSingleValue[A](kvd: KeyValueDefinition[A,COV])(implicit isHCons: Aux[A :: HO, A, HO]):
  KvpSingleValueHead[A, HO, NO, A :: HO, COV] = KvpSingleValueHead[A, HO, NO, A :: HO,COV](kvd, List.empty, this, isHCons)


  def validate(v: ValidationOp[HO]): KvpHListHead[HO, NO, H, HL, T, TL, COV] =
    this.copy(validations = v :: validations)

}