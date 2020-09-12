package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Length, Prepend, Split, Tupler}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Nat, Succ}

object KvpCollection {
//  type Empty[_] = Any
  def headManifest[ALG[_], A](kvpCollection: KvpCollection[ALG, A]): Option[Manifest[A]] = {
    kvpCollection match {
      case w: WrappedEncoding[ALG, A] @unchecked                  => Some(w.manifestOfA)
      case c: KvpCoproductCollectionHead[ALG, A, _, _] @unchecked => Some(c.manifestOfHead)
      case s: KvpSingleValueHead[ALG, A, _, _, _] @unchecked => {
        s.head match {
          case Left(keyDef) => Some(keyDef.manifestOfA)
          case Right(coll)  => headManifest(coll)
        }
      }
      case _ => None
    }
  }
}

/** Base Trait for anything that represents a collection KeyValue Pair.
  *  The direct subclasses are:
  *  [[KvpHListCollection]] - Base Class when the A is an HList.  With this we have the ability to
  *    construct a new KvpCollection with a new item at the head (aka cons, aka ::) and we have the ability
  *    to construct a new KvpCollection prepending another HList (aka :::)
  *  [[KvpCoproduct]] - Base Class when the A is a Coproduct, aka, OneOf.  This can eventually be wrapped by a value
  *     with represents a single trait.
  *   [[WrappedEncoding]] - This allows us to wrap an HList type (such as String :: Ing :: HNil) or a Coprodcut type
  *    (such as Cat :+: Dog :+: CNil) into more scala friendly types (such as Person(name: String, age: Int)
  *     or (trait Animal) respectively.
  *
  **/
sealed trait KvpCollection[ALG[_], A]

/**
  * This allows us to wrap an HList type (such as String :: Ing :: HNil) or a Coprodcut type
  *    (such as Cat :+: Dog :+: CNil) into more scala friendly types (such as Person(name: String, age: Int)
  *     or (trait Animal) respectively.
  * @tparam ALG the GADT Context
  * @tparam A The new value which is a switch in context from the Generic type values.
  */
sealed trait WrappedEncoding[ALG[_], A] extends KvpCollection[ALG, A] {
  implicit val manifestOfA: Manifest[A]
  def asValue: KvpCollectionValue[ALG, A] = KvpCollectionValue(this, List.empty)
}

case class KvpWrappedHList[ALG[_], A: Manifest, XS <: HList, XSL <: Nat](
  wrappedEncoding: KvpHListCollection[ALG, XS, XSL],
  fHtoA: XS => A,
  fAtoH: A => XS,
  validations: List[ValidationOp[A]]
) extends WrappedEncoding[ALG, A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

case class KvpWrappedCoproduct[ALG[_], A: Manifest, C <: Coproduct](
  wrappedEncoding: KvpCoproduct[ALG, C],
  fCtoA: C => A,
  fAtoC: A => C,
  validationOp: List[ValidationOp[A]])
    extends WrappedEncoding[ALG, A] {
  val manifestOfA: Manifest[A] = manifest[A]

}

sealed trait KvpCoproduct[ALG[_], C <: Coproduct] extends KvpCollection[ALG, C] {
  self =>

  def :+:[A: Manifest](
    head: KvpCollection[ALG, A]): KvpCoproductCollectionHead[ALG, A, C, A :+: C] = {
    KvpCoproductCollectionHead[ALG, A, C, A :+: C](head, this)
  }

  /** Convert a Coproduct into an object with validation on the object. */
  def toSuperclassOf[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, C]): KvpWrappedCoproduct[ALG, A, C] =
    KvpWrappedCoproduct[ALG, A, C](self, gen.from, gen.to, validation.toList)

  /** Convert a Coproduct into an object */
  def toSuperclassOf[A: Manifest](implicit gen: Generic.Aux[A, C]): KvpWrappedCoproduct[ALG, A, C] =
    toSuperclassOf[A]()

}

/**
  * The starting point for a Coproduct.
  * @tparam ALG The GADT context.
  */
case class KvpCoNil[ALG[_]]() extends KvpCoproduct[ALG, CNil] {}

/**
  *
  * @param kvpTail The rest of the coproduct
  * @tparam ALG The GADT context.
  * @tparam A The head (or most left part) of the coproduct (of the values of Coproduct, the one in particular this instance represents)
  * @tparam C The remaining part of the coproduct.  This class
  * @tparam O The new Corpoduct after the A is prepended to C.
  */
case class KvpCoproductCollectionHead[ALG[_], A: Manifest, C <: Coproduct, O <: A :+: C](
  kvpCollection: KvpCollection[ALG, A],
  kvpTail: KvpCoproduct[ALG, C],
) extends KvpCoproduct[ALG, O] {
  val manifestOfHead: Manifest[A] = manifest[A]
}

/**
  * Contains a collection of key-value pairs.
  *
  * @tparam L The HList this value represents.
  * @tparam LL The length of this HList
  */
sealed abstract class KvpHListCollection[ALG[_], L <: HList, LL <: Nat]
    extends KvpCollection[ALG, L] {

  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, L]): KvpWrappedHList[ALG, A, L, LL] =
    KvpWrappedHList(this, gen.from, gen.to, validation.toList)

  def convert[A: Manifest](implicit gen: Generic.Aux[A, L]): KvpWrappedHList[ALG, A, L, LL] =
    convert[A]()

  def tupled[Tup <: Product: Manifest](
    implicit tupler: Tupler.Aux[L, Tup],
    gen: Generic[Tup]
  ): KvpWrappedHList[ALG, Tup, L, LL] = tupled[Tup]()

  def tupled[Tup <: Product: Manifest](tupledValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[L, Tup],
    gen: Generic[Tup]
  ): KvpWrappedHList[ALG, Tup, L, LL] =
    KvpWrappedHList[ALG, Tup, L, LL](this, (h: L) => tupler.apply(h), (t: Tup) => {
      val out = gen.to(t).asInstanceOf[L]
      out
    }, tupledValidations.toList)

  def xmap[A: Manifest, B](f: B => A, g: A => B, validation: ValidationOp[A]*)(
    implicit isEqual: (B :: HNil) =:= L
  ): KvpWrappedHList[ALG, A, L, LL] = {
    type T = B :: HNil
    val witness = implicitly[(B :: HNil) =:= L]
    val f2: L => A = h => f(witness.flip(h).head)
    val g2: A => L = a => witness(g(a) :: HNil)
    hListXmap[A](f2, g2, validation.toList: _*)
  }

  def encodedHead[B: Manifest](validation: ValidationOp[B]*)(
    implicit isEqual: (B :: HNil) =:= L
  ) =
    xmap[B, B](identity, identity)

  def xmapTup[A: Manifest, Tup <: Product](f: Tup => A, g: A => Tup, validations: ValidationOp[A]*)(
    implicit tupler: Tupler.Aux[L, Tup],
    gen: Generic[Tup]
  ): KvpWrappedHList[ALG, A, L, LL] = {
    val f2: L => A = (h: L) => f(tupler.apply(h))
    val g2: A => L = (a: A) => gen.to(g(a)).asInstanceOf[L]
    KvpWrappedHList(this, f2, g2, validations.toList)
  }

  def hListXmap[A: Manifest](
    f: L => A,
    g: A => L,
    validations: ValidationOp[A]*): KvpWrappedHList[ALG, A, L, LL] = {
    KvpWrappedHList(this, f, g, validations.toList)
  }

  def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHListCollection[ALG, HP, NP])(
    implicit prepend: Prepend.Aux[HP, L, HO],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO, NO],
    split: Split.Aux[HO, NP, HP, L]
  ): KvpHListCollection[ALG, HO, NO] = prependHList(kvp)

  def prependHList[NL <: HList, NLL <: Nat, L2 <: HList, LL2 <: Nat](
    kvp: KvpHListCollection[ALG, L2, LL2])(
    implicit prepend: hlist.Prepend.Aux[L2, L, NL],
    lengthP: Length.Aux[L2, LL2],
    length: Length.Aux[NL, NLL],
    split: Split.Aux[NL, LL2, L2, L]): KvpHListCollection[ALG, NL, NLL] =
    KvpHListCollectionHead[ALG, NL, NLL, L2, LL2, L, LL](kvp, this, prepend, split, List.empty)

  def consKeyDefinition[A: Manifest](v: KeyDefinition[ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[ALG, A, L, LL, A :: L] =
    KvpSingleValueHead(Left(v), List.empty, this, isHCons)

  def consKvpCollection[X: Manifest](kvpCollection: KvpCollection[ALG, X])(
    implicit isHCons: IsHCons.Aux[X :: L, X, L]): KvpSingleValueHead[ALG, X, L, LL, X :: L] =
    KvpSingleValueHead(Right(kvpCollection), List.empty, this, isHCons)

  def >>:[A: Manifest](v: KeyDefinition[ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[ALG, A, L, LL, A :: L] =
    consKeyDefinition(v)

  /** Alias for prependCoproduct */
  def ::[X: Manifest](coproduct: KvpCollection[ALG, X])(
    implicit isHCons: IsHCons.Aux[X :: L, X, L]): KvpSingleValueHead[ALG, X, L, LL, X :: L] =
    consKvpCollection(coproduct)

  /**
    * Use this operator when you want to prefix a Data Type.
    * @param input The Key, Value Pair to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def ::[A: Manifest](input: (String, ALG[A]))(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[ALG, A, L, LL, A :: L] =
    consKeyDefinition(KeyDefinition(input._1, Right(input._2), None, None))(manifest[A], isHCons)

  /**
    * Use this operator when you want to prefix a data type with a description
    * @param input The Key, Value Pair and Description, Example to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def ::[A: Manifest](input: (String, ALG[A], String, A))(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[ALG, A, L, LL, A :: L] =
    consKeyDefinition(KeyDefinition(input._1, Right(input._2), Some(input._3), Some(input._4)))(
      manifest[A],
      isHCons)

  /**
    * Prefixes one of the Algebra types to this HLIst
    * @param input The Key, Value Pair to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def :<:[A: Manifest](input: (String, HigherOrderValue[ALG, A]))(
    implicit isHCons: Aux[A :: L, A, L]): KvpSingleValueHead[ALG, A, L, LL, A :: L] =
    consKeyDefinition(KeyDefinition(input._1, Left(input._2), None, None))(manifest[A], isHCons)

  def :<:[A: Manifest](input: (String, HigherOrderValue[ALG, A], String, A))(
    implicit isHCons: Aux[A :: L, A, L]): KvpSingleValueHead[ALG, A, L, LL, A :: L] =
    consKeyDefinition(new KeyDefinition(input._1, Left(input._2), Some(input._3), Some(input._4)))(
      manifest[A],
      isHCons)

}

/** The Nil as in an empty HList.
  */
case class KvpNil[ALG[_]]() extends KvpHListCollection[ALG, HNil, Nat._0]

/** The head of the HList has a known KeyValueDefinition. */
final case class KvpSingleValueHead[ALG[_], X, XS <: HList, XSL <: Nat, OUT <: X :: XS](
  head: Either[KeyDefinition[ALG, X], KvpCollection[ALG, X]],
  validations: List[ValidationOp[OUT]],
  tail: KvpHListCollection[ALG, XS, XSL],
  isHCons: IsHCons.Aux[OUT, X, XS]
) extends KvpHListCollection[ALG, OUT, Succ[XSL]] {

  def validate(v: ValidationOp[OUT]): KvpSingleValueHead[ALG, X, XS, XSL, OUT] =
    this.copy(validations = v :: validations)
}

/** This is a group of KvpHList that are grouped and the validations match the entire group.  */
final case class KvpHListCollectionHead[
  ALG[_],
  HO <: HList,
  NO <: Nat,
  H <: HList,
  HL <: Nat,
  T <: HList,
  TL <: Nat](
  head: KvpHListCollection[ALG, H, HL],
  tail: KvpHListCollection[ALG, T, TL],
  prepend: Prepend.Aux[H, T, HO],
  split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
  validations: List[ValidationOp[HO]]
) extends KvpHListCollection[ALG, HO, NO] {

  def validate(v: ValidationOp[HO]): KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL] =
    this.copy(validations = v :: validations)

}
