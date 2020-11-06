package com.bones.data.template

import cats._
import cats.implicits._
import com.bones.data._
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, Nat}

trait KvpCollectionTransformation[K, ALG[_], OUT[_]] {

  implicit def applicativeOfOut: Applicative[OUT]

  def primitiveEncoder[A](keyDefinition: KeyDefinition[K, ALG, A]): OUT[A]

  def fromKvpCollection[A](kvpCollection: KvpCollection[K, ALG, A]): OUT[A] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[K, ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
      case kvp: KvpWrappedCoproduct[K, ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpCoNil[K, ALG]                             => kvpCoNil(kvp)
      case kvp: KvpCoproductCollectionHead[K, ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp).asInstanceOf[OUT[A]]
      case kvp: KvpSingleValueHead[K, ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp).asInstanceOf[OUT[A]]
      case kvp: KvpHListCollectionHead[K, ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp).asInstanceOf[OUT[A]]
      case kvp: KvpNil[K, ALG] => kvpNil(kvp).asInstanceOf[OUT[A]]
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[K, ALG, A, H, HL]): OUT[A] = {
    val wrappedF = fromKvpCollection(wrappedHList.wrappedEncoding)
    applicativeOfOut.map(wrappedF)(wrappedHList.fHtoA)
  }

  def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[K, ALG, A, C]): OUT[A] = {
    val wrappedF = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    applicativeOfOut.map(wrappedF)(wrappedCoproduct.fCtoA)
  }

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[K, ALG, HO, NO, H, HL, T, TL]): OUT[HO] = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    val combine = (h: H, t: T) => kvp.prepend(h, t)
    applicativeOfOut.map2(head, tail)(combine)
  }

  def kvpNil(kvp: KvpNil[K, ALG]): OUT[HList] = Applicative[OUT].pure(HNil)

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[K, ALG, H, T, TL, O]): OUT[O] = {
    val head: OUT[H] = kvp.head match {
      case Left(value) => primitiveEncoder(value)
      case Right(collection) =>
        fromKvpCollection(collection)
    }
    val tail = fromKvpCollection(kvp.tail)
    implicit val hCons = kvp.isHCons
    val combine = (h: H, t: T) => (h :: t).asInstanceOf[O]
    applicativeOfOut.map2(head, tail)(combine)
  }

  def kvpCoNil(kvpCoNil: KvpCoNil[K, ALG]): OUT[CNil] =
    sys.error("Unreachable (I hope)")

  def kvpCoproductCollectionHead[A, C <: Coproduct, O <: A :+: C](
    kvpCoproductCollectionHead: KvpCoproductCollectionHead[K, ALG, A, C, O]): OUT[O]

// TODO: Struggling with a generic implementation for kvpCoproductCollectionHead.
//       Will have to do some non-generic implementations in order to figure it out.
//
//  {
//    val headF = fromKvpCollection(kvpCoproductCollectionHead.kvpCollection)
//    val tailF = fromKvpCollection(kvpCoproductCollectionHead.kvpTail)
//
//    val fac: OUT[A :+: C] = ???
//
//    val f: A :+: C => O = (ac) => {
//      ac match {
//        case Inl(a) =>
//      }
//    }
//    val fOut: OUT[A :+: C => O] = ???
//
//    def f(a: Inl[A, C], c: Inr[A, C]): O = {}
//
//    applicativeOfOut.map2(headF, tailF)()
//
//
//
//    applicativeOfOut.ap[A :+: C, O](fOut)(fac)
//  }

}
