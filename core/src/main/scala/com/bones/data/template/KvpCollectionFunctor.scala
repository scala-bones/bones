package com.bones.data.template

import com.bones.data._
import shapeless.{::, Coproduct, HList, HNil, Nat}

trait KvpCollectionFunctor[K, ALG[_], F[_]] {
  def fromKvpCollection[A](kvpCollection: KvpCollection[K, ALG, A]): F[A] = {
    kvpCollection match {
      case kvp: KvpWrappedHList[K, ALG, a, h, n] @unchecked =>
        kvpWrappedHList(kvp).asInstanceOf[F[A]]
      case kvp: KvpWrappedCoproduct[K, ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpSingleValueHead[K, ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp).asInstanceOf[F[A]]
      case kvp: KvpHListCollectionHead[K, ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp).asInstanceOf[F[A]]
      case kvp: KvpNil[K, ALG]          => kvpNil(kvp)
      case kvp: KvpCoproduct[K, ALG, c] => kvpCoproduct(kvp).asInstanceOf[F[A]]
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[K, ALG, A, H, HL]
  ): F[A]

  def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[K, ALG, A, C]
  ): F[A]

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[K, ALG, HO, NO, H, HL, T, TL]
  ): F[HO]

  def kvpNil(kvp: KvpNil[K, ALG]): F[HNil]

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[K, ALG, H, T, TL, O]
  ): F[O]

  def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[K, ALG, C]): F[C]
}
