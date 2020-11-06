package com.bones.data.template

import com.bones.data._
import shapeless.{::, Coproduct, HList, Nat}

trait KvpCollectionMatch[K, ALG[_], B] {
  def fromKvpCollection[A](kvpCollection: KvpCollection[K, ALG, A]): B = {
    kvpCollection match {
      case kvp: KvpWrappedHList[K, ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
      case kvp: KvpWrappedCoproduct[K, ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpSingleValueHead[K, ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp)
      case kvp: KvpHListCollectionHead[K, ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp)
      case kvp: KvpNil[K, ALG]          => kvpNil(kvp)
      case kvp: KvpCoproduct[K, ALG, c] => kvpCoproduct(kvp)
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](wrappedHList: KvpWrappedHList[K, ALG, A, H, HL]): B

  def kvpWrappedCoproduct[A, C <: Coproduct](wrappedCoproduct: KvpWrappedCoproduct[K, ALG, A, C]): B

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[K, ALG, HO, NO, H, HL, T, TL]): B

  def kvpNil(kvp: KvpNil[K, ALG]): B

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[K, ALG, H, T, TL, O]): B

  def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[K, ALG, C]): B
}
