package com.bones.data.template

import com.bones.data.{
  KeyDefinition,
  KvpCoNil,
  KvpCollection,
  KvpCoproductCollectionHead,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList
}
import shapeless.{:+:, ::, CNil, Coproduct, HList, Inl, Inr, Nat}

trait KvpCollectionEncoder[ALG[_], OUT] {

  def primitiveEncoder[A](keyDefinition: KeyDefinition[ALG, A]): A => OUT
  def combine(a: OUT, b: OUT): OUT
  def empty: OUT
  def coproductTypeKey: String
  def addStringField(element: OUT, name: String, value: String): OUT

  def fromKvpCollection[A](kvpCollection: KvpCollection[ALG, A]): A => OUT = {
    kvpCollection match {
      case kvp: KvpWrappedHList[ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
      case kvp: KvpWrappedCoproduct[ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
      case kvp: KvpCoNil[ALG]                             => kvpCoNil(kvp)
      case kvp: KvpCoproductCollectionHead[ALG, a, c, o] =>
        kvpCoproductCollectionHead[a, c, o](kvp).asInstanceOf[A => OUT]
      case kvp: KvpSingleValueHead[ALG, A, t, tl, ht] @unchecked =>
        kvpSingleValueHead[A, t, tl, ht](kvp).asInstanceOf[A => OUT]
      case kvp: KvpHListCollectionHead[ALG, ho, no, h, hl, t, tl] @unchecked =>
        kvpHListCollectionHead(kvp).asInstanceOf[A => OUT]
      case kvp: KvpNil[ALG] => kvpNil(kvp)
    }
  }

  def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL]): A => OUT = {
    val wrappedF = fromKvpCollection(wrappedHList.wrappedEncoding)
    (a: A) =>
      {
        val h = wrappedHList.fAtoH(a)
        wrappedF(h)
      }
  }

  def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): A => OUT = {
    val wrappedF = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    (a: A) =>
      {
        val c = wrappedCoproduct.fAtoC(a)
        wrappedF(c)
      }
  }

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): HO => OUT = {
    val headF = fromKvpCollection(kvp.head)
    val tailF = fromKvpCollection(kvp.tail)
    implicit val split = kvp.split
    (ho: HO) =>
      {
        val (headInput, tailInput) = split(ho)
        val headResult = headF(headInput)
        val tailResult = tailF(tailInput)
        combine(headResult, tailResult)
      }
  }

  def kvpNil(kvp: KvpNil[ALG]): HList => OUT = (_: HList) => empty

  def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): O => OUT = {
    val headF: H => OUT = kvp.head match {
      case Left(value) => primitiveEncoder(value)
      case Right(collection) =>
        fromKvpCollection(collection)
    }
    val tailF = fromKvpCollection(kvp.tail)
    (o: O) =>
      {
        val headInput = kvp.isHCons.head(o)
        val tailInput = kvp.isHCons.tail(o)
        val headResult = headF(headInput)
        val tailResult = tailF(tailInput)
        combine(headResult, tailResult)
      }
  }

  def kvpCoNil(kvpCoNil: KvpCoNil[ALG]): CNil => OUT = (_: CNil) => empty

  def kvpCoproductCollectionHead[A, C <: Coproduct, O <: A :+: C](
    kvpCoproductCollectionHead: KvpCoproductCollectionHead[ALG, A, C, O]): O => OUT = {
    val headF = fromKvpCollection(kvpCoproductCollectionHead.kvpCollection)
    val tailF = fromKvpCollection(kvpCoproductCollectionHead.kvpTail)
    (o: A :+: C) =>
      {
        o match {
          case Inl(a) => {
            val out = headF(a)
            val manifest = KvpCollection.headManifest(kvpCoproductCollectionHead.kvpCollection)
            val typeName = manifest.map(_.runtimeClass.getSimpleName).getOrElse("unknown")
            addStringField(out, coproductTypeKey, typeName)
          }
          case Inr(tail) => tailF(tail)
        }
      }
  }

}
