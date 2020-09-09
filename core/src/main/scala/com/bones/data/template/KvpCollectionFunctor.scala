package com.bones.data.template

import com.bones.data._
import shapeless.{::, Coproduct, HList, HNil, Nat}

trait KvpCollectionFunctor[ALG[_], F[_]] {
  def fromKvpCollection[A](kvpCollection: KvpCollection[ALG, A]): F[A] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp).asInstanceOf[F[A]]
      case kvp: KvpWrappedCoproduct[ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpSingleValueHead[ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp).asInstanceOf[F[A]]
      case kvp: KvpHListCollectionHead[ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp).asInstanceOf[F[A]]
      case kvp: KvpNil[ALG]          => kvpNil(kvp)
      case kvp: KvpCoproduct[ALG, c] => kvpCoproduct(kvp).asInstanceOf[F[A]]
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](wrappedHList: KvpWrappedHList[ALG, A, H, HL]): F[A]

  def kvpWrappedCoproduct[A, C <: Coproduct](wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): F[A]

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): F[HO]

  def kvpNil(kvp: KvpNil[ALG]): F[HNil]

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): F[O]

  def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[ALG, C]): F[C]
}
