package com.bones.data

import com.bones.Util.CanBeOmitted
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.Nat._0
import shapeless.ops.hlist
import shapeless.ops.hlist.IsHCons.Aux
import shapeless.ops.hlist.{IsHCons, Length, Prepend, Split, Tupler}
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Nat, Succ, UnaryTCConstraint}

object KvpCollection {
//  type Empty[_] = Any
  def headTypeName[K, ALG[_], A](kvpCollection: KvpCollection[K, ALG, A]): Option[String] = {
    kvpCollection match {
      case w: WrappedEncoding[K, ALG, A] @unchecked                  => Some(w.typeNameOfA)
      case c: KvpCoproductCollectionHead[K, ALG, A, _, _] @unchecked => Some(c.typeNameOfA)
      case s: KvpSingleValueHead[K, ALG, A, _, _, _] @unchecked => {
        s.head match {
          case Left(keyDef) => Some(keyDef.typeName)
          case Right(coll)  => headTypeName(coll)
        }
      }
      case _ => None
    }
  }

  def empty[K, ALG[_]]: KvpNil[K, ALG] = KvpNil[K, ALG]()
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
sealed trait KvpCollection[K, ALG[_], A] {
  val typeNameOfA: String

  def keyMapKvpCollection[KK](f: K => KK): KvpCollection[KK, ALG, A] =
    this match {
      case co: KvpCoproduct[K, ALG, A] @unchecked =>
        co.keyMapCoproduct(f)
      case co: KvpHListCollection[K, ALG, A, ll] @unchecked =>
        co.keyMapKvpCollection(f)
      case w: WrappedEncoding[K, ALG, A] @unchecked =>
        w.keyMapKvpCollection(f)
    }

  def algMapKvpCollection[ALG2[_]](f: ALG[_] => ALG2[_]): KvpCollection[K, ALG2, A] = {
    this match {
      case co: KvpCoproduct[K, ALG, A] @unchecked =>
        co.algMapCoproduct(f)
      case co: KvpHListCollection[K, ALG, A, ll] @unchecked =>
        co.algMapKvpCollection(f)
      case w: WrappedEncoding[K, ALG, A] @unchecked =>
        w.algMapWrappedEncoding(f)
    }
  }
}

/**
  * This allows us to wrap an HList type (such as String :: Ing :: HNil) or a Coprodcut type
  *    (such as Cat :+: Dog :+: CNil) into more scala friendly types (such as Person(name: String, age: Int)
  *     or (trait Animal) respectively.
  * @tparam ALG the GADT Context
  * @tparam A The new value which is a switch in context from the Generic type values.
  */
sealed trait WrappedEncoding[K, ALG[_], A] extends KvpCollection[K, ALG, A] {
  def asValue: KvpCollectionValue[K, ALG, A] = KvpCollectionValue(this, typeNameOfA, List.empty)
  def keyMapWrappedEncoding[KK](f: K => KK): WrappedEncoding[KK, ALG, A] =
    this match {
      case w: KvpWrappedCoproduct[K, ALG, A, c]   => w.keyMapKvpWrappedCoproduct(f)
      case w: KvpWrappedHList[K, ALG, A, xs, xsl] => w.keyMapKvpWrappedHList(f)
    }
  def algMapWrappedEncoding[ALG2[_]](f: ALG[_] => ALG2[_]): WrappedEncoding[K, ALG2, A] = {
    this match {
      case w: KvpWrappedCoproduct[K, ALG, A, c]   => w.algMapKvpWrappedCoproduct(f)
      case w: KvpWrappedHList[K, ALG, A, xs, xsl] => w.algMapKvpWrappedHList(f)
    }
  }

}

case class KvpWrappedHList[K, ALG[_], A, XS <: HList, XSL <: Nat](
  wrappedEncoding: KvpHListCollection[K, ALG, XS, XSL],
  typeNameOfA: String,
  fHtoA: XS => A,
  fAtoH: A => XS,
  validations: List[ValidationOp[A]]
) extends WrappedEncoding[K, ALG, A] {
  def keyMapKvpWrappedHList[KK](f: K => KK): KvpWrappedHList[KK, ALG, A, XS, XSL] =
    KvpWrappedHList(
      wrappedEncoding.keyMapKvpHListCollection(f),
      typeNameOfA,
      fHtoA,
      fAtoH,
      validations)
  def algMapKvpWrappedHList[ALG2[_]](f: ALG[_] => ALG2[_]): KvpWrappedHList[K, ALG2, A, XS, XSL] =
    KvpWrappedHList(
      wrappedEncoding.algMapKvpHListCollection(f),
      typeNameOfA,
      fHtoA,
      fAtoH,
      validations)
}

case class KvpWrappedCoproduct[K, ALG[_], A, C <: Coproduct](
  wrappedEncoding: KvpCoproduct[K, ALG, C],
  typeNameOfA: String,
  fCtoA: C => A,
  fAtoC: A => C,
  validationOp: List[ValidationOp[A]])
    extends WrappedEncoding[K, ALG, A] {
  def keyMapKvpWrappedCoproduct[KK](f: K => KK): KvpWrappedCoproduct[KK, ALG, A, C] =
    KvpWrappedCoproduct[KK, ALG, A, C](
      wrappedEncoding.keyMapCoproduct(f),
      typeNameOfA,
      fCtoA,
      fAtoC,
      validationOp
    )

  def algMapKvpWrappedCoproduct[ALG2[_]](f: ALG[_] => ALG2[_]): KvpWrappedCoproduct[K, ALG2, A, C] =
    KvpWrappedCoproduct[K, ALG2, A, C](
      wrappedEncoding.algMapCoproduct(f),
      typeNameOfA,
      fCtoA,
      fAtoC,
      validationOp
    )
}

sealed trait KvpCoproduct[K, ALG[_], C <: Coproduct] extends KvpCollection[K, ALG, C] {
  self =>

  def keyMapCoproduct[KK](f: K => KK): KvpCoproduct[KK, ALG, C] = {
    this match {
      case co: KvpCoNil[K, ALG] => co.keyMapKvpCoNil(f).asInstanceOf[KvpCoproduct[KK, ALG, C]]
      case co: KvpCoproductCollectionHead[K, ALG, a, c, o] =>
        co.keyMapKvpCoproductCollectionHead(f).asInstanceOf[KvpCoproduct[KK, ALG, C]]
    }
  }

  def algMapCoproduct[ALG2[_]](f: ALG[_] => ALG2[_]): KvpCoproduct[K, ALG2, C] = {
    this match {
      case co: KvpCoNil[K, ALG] => co.algMapKvpCoNil.asInstanceOf[KvpCoproduct[K, ALG2, C]]
      case co: KvpCoproductCollectionHead[K, ALG, a, C, o] =>
        co.algMapKvpCoproductCollectionHead(f).asInstanceOf[KvpCoproduct[K, ALG2, C]]
    }
  }

  def :+:[A: Manifest](
    head: KvpCollection[K, ALG, A]): KvpCoproductCollectionHead[K, ALG, A, C, A :+: C] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    KvpCoproductCollectionHead[K, ALG, A, C, A :+: C](head, typeName, this)
  }

  /** Convert a Coproduct into an object with validation on the object. */
  def toSuperclassOf[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, C]): KvpWrappedCoproduct[K, ALG, A, C] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    KvpWrappedCoproduct[K, ALG, A, C](self, typeName, gen.from, gen.to, validation.toList)
  }

  /** Convert a Coproduct into an object */
  def toSuperclassOf[A: Manifest](
    implicit gen: Generic.Aux[A, C]): KvpWrappedCoproduct[K, ALG, A, C] =
    toSuperclassOf[A]()

}

/**
  * The starting point for a Coproduct.
  * @tparam ALG The GADT context.
  */
case class KvpCoNil[K, ALG[_]]() extends KvpCoproduct[K, ALG, CNil] {
  override val typeNameOfA: String = "CNil"
  def keyMapKvpCoNil[KK](f: K => KK): KvpCoNil[KK, ALG] = KvpCoNil[KK, ALG]()

  def algMapKvpCoNil[ALG2[_]]: KvpCoNil[K, ALG2] = KvpCoNil[K, ALG2]()
}

/**
  *
  * @param kvpTail The rest of the coproduct
  * @tparam ALG The GADT context.
  * @tparam A The head (or most left part) of the coproduct (of the values of Coproduct, the one in particular this instance represents)
  * @tparam C The remaining part of the coproduct.  This class
  * @tparam O The new Corpoduct after the A is prepended to C.
  */
case class KvpCoproductCollectionHead[K, ALG[_], A, C <: Coproduct, O <: A :+: C](
  kvpCollection: KvpCollection[K, ALG, A],
  typeNameOfA: String,
  kvpTail: KvpCoproduct[K, ALG, C],
) extends KvpCoproduct[K, ALG, O] {
  def keyMapKvpCoproductCollectionHead[KK](
    f: K => KK): KvpCoproductCollectionHead[KK, ALG, A, C, O] =
    KvpCoproductCollectionHead[KK, ALG, A, C, O](
      kvpCollection.keyMapKvpCollection(f),
      typeNameOfA,
      kvpTail.keyMapCoproduct(f))

  def algMapKvpCoproductCollectionHead[ALG2[_]](
    f: ALG[_] => ALG2[_]): KvpCoproductCollectionHead[K, ALG2, A, C, Nothing] = {
    this.copy(
      kvpCollection = kvpCollection.algMapKvpCollection(f),
      kvpTail = kvpTail.algMapCoproduct(f))
  }
}

/**
  * Contains a collection of key-value pairs.
  *
  * @tparam L The HList this value represents.
  * @tparam LL The length of this HList
  */
sealed abstract class KvpHListCollection[K, ALG[_], L <: HList, LL <: Nat]
    extends KvpCollection[K, ALG, L] {

  def keyMapKvpHListCollection[KK](f: K => KK): KvpHListCollection[KK, ALG, L, LL] =
    this match {
      case n: KvpNil[K, ALG] => n.keyMapKvpNil(f).asInstanceOf[KvpHListCollection[KK, ALG, L, LL]]
      case kvp: KvpHListCollectionHead[K, ALG, ho, no, h, hl, t, tl] =>
        kvp.keyMapKvpHListCollectionHead(f).asInstanceOf[KvpHListCollection[KK, ALG, L, LL]]
      case kvp: KvpSingleValueHead[K, ALG, x, xs, xsl, o] =>
        kvp.keyMapKvpSingleValueHead(f).asInstanceOf[KvpHListCollection[KK, ALG, L, LL]]
    }

  def algMapKvpHListCollection[ALG2[_]](f: ALG[_] => ALG2[_]): KvpHListCollection[K, ALG2, L, LL] =
    this match {
      case n: KvpNil[K, ALG] => n.algMapKvpNil().asInstanceOf[KvpHListCollection[K, ALG2, L, LL]]
      case kvp: KvpHListCollectionHead[K, ALG, ho, no, h, hl, t, tl] =>
        kvp.algMapKvpHListCollectionHead(f).asInstanceOf[KvpHListCollection[K, ALG2, L, LL]]
      case kvp: KvpSingleValueHead[K, ALG, x, xs, xsl, o] =>
        kvp.algMapKvpSingleValueHead(f).asInstanceOf[KvpHListCollection[K, ALG2, L, LL]]
    }

  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, L]): KvpWrappedHList[K, ALG, A, L, LL] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    KvpWrappedHList(this, typeName, gen.from, gen.to, validation.toList)
  }

  def convert[A: Manifest](implicit gen: Generic.Aux[A, L]): KvpWrappedHList[K, ALG, A, L, LL] =
    convert[A]()

  def convertWithName[A](typeName: String, validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, L]): KvpWrappedHList[K, ALG, A, L, LL] =
    KvpWrappedHList(this, typeName, gen.from, gen.to, validation.toList)

  def tupled[Tup <: Product](
    implicit tupler: Tupler.Aux[L, Tup],
    gen: Generic[Tup]
  ): KvpWrappedHList[K, ALG, Tup, L, LL] = tupled[Tup]()

  def tupled[Tup <: Product](tupledValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[L, Tup],
    gen: Generic[Tup]
  ): KvpWrappedHList[K, ALG, Tup, L, LL] = {
    KvpWrappedHList[K, ALG, Tup, L, LL](this, "Tuple", (h: L) => tupler.apply(h), (t: Tup) => {
      val out = gen.to(t).asInstanceOf[L]
      out
    }, tupledValidations.toList)
  }

  def xmap[A: Manifest, B](f: B => A, g: A => B, validation: ValidationOp[A]*)(
    implicit isEqual1: (B :: HNil) =:= L,
    isEqual2: L =:= (B :: HNil)
  ): KvpWrappedHList[K, ALG, A, L, LL] = {
    type T = B :: HNil
    val witness1 = implicitly[(B :: HNil) =:= L]
    val witness2 = implicitly[L =:= (B :: HNil)]
    val f2: L => A = h => f(witness2(h).head)
    val g2: A => L = a => witness1(g(a) :: HNil)
    hListXmap[A](f2, g2, validation.toList: _*)
  }

  def encodedHead[B: Manifest](validation: ValidationOp[B]*)(
    implicit isEqual1: (B :: HNil) =:= L,
    isEqual2: L =:= (B :: HNil)
  ) =
    xmap[B, B](identity, identity)

  def xmapTup[A: Manifest, Tup <: Product](f: Tup => A, g: A => Tup, validations: ValidationOp[A]*)(
    implicit tupler: Tupler.Aux[L, Tup],
    gen: Generic[Tup]
  ): KvpWrappedHList[K, ALG, A, L, LL] = {
    val f2: L => A = (h: L) => f(tupler.apply(h))
    val g2: A => L = (a: A) => gen.to(g(a)).asInstanceOf[L]
    val typeName = manifest[A].runtimeClass.getSimpleName
    KvpWrappedHList(this, typeName, f2, g2, validations.toList)
  }

  def hListXmap[A: Manifest](
    f: L => A,
    g: A => L,
    validations: ValidationOp[A]*): KvpWrappedHList[K, ALG, A, L, LL] = {
    val typeNameOfA = manifest[A].runtimeClass.getSimpleName
    KvpWrappedHList(this, typeNameOfA, f, g, validations.toList)
  }

  def :::[HO <: HList, NO <: Nat, HP <: HList, NP <: Nat](kvp: KvpHListCollection[K, ALG, HP, NP])(
    implicit prepend: Prepend.Aux[HP, L, HO],
    lengthP: Length.Aux[HP, NP],
    length: Length.Aux[HO, NO],
    split: Split.Aux[HO, NP, HP, L]
  ): KvpHListCollection[K, ALG, HO, NO] = prependHList(kvp)

  def prependHList[NL <: HList, NLL <: Nat, L2 <: HList, LL2 <: Nat](
    kvp: KvpHListCollection[K, ALG, L2, LL2])(
    implicit prepend: hlist.Prepend.Aux[L2, L, NL],
    lengthP: Length.Aux[L2, LL2],
    length: Length.Aux[NL, NLL],
    split: Split.Aux[NL, LL2, L2, L]): KvpHListCollection[K, ALG, NL, NLL] = {
    val typeName = "H"
    KvpHListCollectionHead[K, ALG, NL, NLL, L2, LL2, L, LL](
      kvp,
      typeName,
      this,
      prepend,
      split,
      List.empty)
  }

  def consKeyDefinition[A: Manifest](v: KeyDefinition[K, ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[K, ALG, A, L, LL, A :: L] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    KvpSingleValueHead(Left(v), typeName, List.empty, this, isHCons)
  }

  def consKvpCollection[X: Manifest](kvpCollection: KvpCollection[K, ALG, X])(
    implicit isHCons: IsHCons.Aux[X :: L, X, L]): KvpSingleValueHead[K, ALG, X, L, LL, X :: L] = {
    val typeName = manifest[X].runtimeClass.getSimpleName
    KvpSingleValueHead(Right(kvpCollection), typeName, List.empty, this, isHCons)
  }

  def >>:[A: Manifest](v: KeyDefinition[K, ALG, A])(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[K, ALG, A, L, LL, A :: L] =
    consKeyDefinition(v)

  /** Alias for prependCoproduct */
  def ::[X: Manifest](coproduct: KvpCollection[K, ALG, X])(
    implicit isHCons: IsHCons.Aux[X :: L, X, L]): KvpSingleValueHead[K, ALG, X, L, LL, X :: L] =
    consKvpCollection(coproduct)

  /**
    * Use this operator when you want to prefix a Data Type.
    * @param input The Key, Value Pair to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def ::[A: Manifest](input: (K, ALG[A]))(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[K, ALG, A, L, LL, A :: L] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    consKeyDefinition(KeyDefinition[K, ALG, A](input._1, Right(input._2), typeName, None, None))(
      manifest[A],
      isHCons)
  }

  /**
    * Use this operator when you want to prefix a data type with a description
    * @param input The Key, Value Pair and Description, Example to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def ::[A: Manifest](input: (K, ALG[A], String, A))(
    implicit isHCons: IsHCons.Aux[A :: L, A, L]): KvpSingleValueHead[K, ALG, A, L, LL, A :: L] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    consKeyDefinition(
      KeyDefinition(input._1, Right(input._2), typeName, Some(input._3), Some(input._4)))(
      manifest[A],
      isHCons)
  }

  /**
    * Prefixes one of the Algebra types to this HLIst
    * @param input The Key, Value Pair to Add to the front of the KvpHList
    * @param isHCons The implied ability to cons (and unapply) A to and from H
    * @tparam A The wrapped type
    * @return KvpSingleValueHead prefixed to this HList
    */
  def :<:[A: Manifest](input: (K, HigherOrderValue[K, ALG, A]))(
    implicit isHCons: Aux[A :: L, A, L]): KvpSingleValueHead[K, ALG, A, L, LL, A :: L] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    consKeyDefinition(KeyDefinition(input._1, Left(input._2), typeName, None, None))(
      manifest[A],
      isHCons)
  }

  def :<:[A: Manifest](input: (K, HigherOrderValue[K, ALG, A], String, A))(
    implicit isHCons: Aux[A :: L, A, L]): KvpSingleValueHead[K, ALG, A, L, LL, A :: L] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    consKeyDefinition(
      new KeyDefinition(input._1, Left(input._2), typeName, Some(input._3), Some(input._4)))(
      manifest[A],
      isHCons)
  }

}

/** The Nil as in an empty HList.
  */
case class KvpNil[K, ALG[_]]() extends KvpHListCollection[K, ALG, HNil, Nat._0] {
  override val typeNameOfA: String = "KvpNil"

  def keyMapKvpNil[KK](f: K => KK): KvpNil[KK, ALG] = KvpNil[KK, ALG]()
  def algMapKvpNil[ALG2[_]]() = KvpNil[K, ALG2]()
}

/** The head of the HList has a known KeyValueDefinition. */
final case class KvpSingleValueHead[K, ALG[_], X, XS <: HList, XSL <: Nat, OUT <: X :: XS](
  head: Either[KeyDefinition[K, ALG, X], KvpCollection[K, ALG, X]],
  typeNameOfA: String,
  validations: List[ValidationOp[OUT]],
  tail: KvpHListCollection[K, ALG, XS, XSL],
  isHCons: IsHCons.Aux[OUT, X, XS]
) extends KvpHListCollection[K, ALG, OUT, Succ[XSL]] {

  def keyMapKvpSingleValueHead[KK](f: K => KK): KvpHListCollection[KK, ALG, OUT, Succ[XSL]] = {
    val newHead = head match {
      case Left(keyDef) => Left(keyDef.keyMap(f))
      case Right(kvp)   => Right(kvp.keyMapKvpCollection(f))
    }
    val newTail = tail.keyMapKvpHListCollection(f)
    KvpSingleValueHead(newHead, typeNameOfA, validations, newTail, isHCons)
  }

  def algMapKvpSingleValueHead[ALG2[_]](
    f: ALG[_] => ALG2[_]): KvpHListCollection[K, ALG2, OUT, Succ[XSL]] = {
    val newHead = head match {
      case Left(keyDef) => Left(keyDef.algMap(f))
      case Right(kvp)   => Right(kvp.algMapKvpCollection(f))
    }
    val newTail = tail.algMapKvpHListCollection(f)
    KvpSingleValueHead(newHead, typeNameOfA, validations, newTail, isHCons)
  }

  def validate(v: ValidationOp[OUT]): KvpSingleValueHead[K, ALG, X, XS, XSL, OUT] =
    this.copy(validations = v :: validations)
}

/** This is a group of KvpHList that are grouped and the validations match the entire group.  */
final case class KvpHListCollectionHead[
  K,
  ALG[_],
  HO <: HList,
  NO <: Nat,
  H <: HList,
  HL <: Nat,
  T <: HList,
  TL <: Nat](
  head: KvpHListCollection[K, ALG, H, HL],
  typeNameOfA: String,
  tail: KvpHListCollection[K, ALG, T, TL],
  prepend: Prepend.Aux[H, T, HO],
  split: Split.Aux[HO, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
  validations: List[ValidationOp[HO]]
) extends KvpHListCollection[K, ALG, HO, NO] {

  def validate(v: ValidationOp[HO]): KvpHListCollectionHead[K, ALG, HO, NO, H, HL, T, TL] =
    this.copy(validations = v :: validations)

  def keyMapKvpHListCollectionHead[KK](
    f: K => KK): KvpHListCollectionHead[KK, ALG, HO, NO, H, HL, T, TL] =
    this.copy(head = head.keyMapKvpHListCollection(f), tail = tail.keyMapKvpHListCollection(f))

  def algMapKvpHListCollectionHead[ALG2[_]](
    f: ALG[_] => ALG2[_]): KvpHListCollectionHead[K, ALG2, HO, NO, H, HL, T, TL] =
    this.copy(head = head.algMapKvpHListCollection(f), tail = tail.algMapKvpHListCollection(f))
}
