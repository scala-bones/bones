package com.bones.data.template

import com.bones.data._
import shapeless.{::, Coproduct, HList, Nat}

trait KvpCollectionMatch[ALG[_], B] {
  def fromKvpCollection[A](kvpCollection: KvpCollection[ALG, A]): B = {
    kvpCollection match {
      case kvp: KvpWrappedHList[ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
      case kvp: KvpWrappedCoproduct[ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpSingleValueHead[ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp)
      case kvp: KvpHListCollectionHead[ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp)
      case kvp: KvpNil[ALG]          => kvpNil(kvp)
      case kvp: KvpCoproduct[ALG, c] => kvpCoproduct(kvp)
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](wrappedHList: KvpWrappedHList[ALG, A, H, HL]): B

  def kvpWrappedCoproduct[A, C <: Coproduct](wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): B

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): B

  def kvpNil(kvp: KvpNil[ALG]): B

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): B

  def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[ALG, C]): B
}
