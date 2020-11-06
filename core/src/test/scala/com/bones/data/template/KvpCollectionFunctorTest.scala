package com.bones.data.template

import com.bones.data.{
  KvpCoproduct,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList
}
import com.bones.data.values.DefaultValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import shapeless.{Coproduct, HList, HNil, Nat, ::}

class KvpCollectionFunctorTest extends AnyFunSuite with Matchers {

  test("extend functor") {

    case class KvpCollectionFunctorImplTest()
        extends KvpCollectionFunctor[String, DefaultValues, String => *] {
      override def kvpNil(kvp: KvpNil[String, DefaultValues]): String => HNil = _ => HNil

      override def kvpWrappedHList[A, H <: HList, HL <: Nat](
        wrappedHList: KvpWrappedHList[String, DefaultValues, A, H, HL]): String => A = ???

      override def kvpWrappedCoproduct[A, C <: Coproduct](
        wrappedCoproduct: KvpWrappedCoproduct[String, DefaultValues, A, C]): String => A = ???

      override def kvpHListCollectionHead[
        HO <: HList,
        NO <: Nat,
        H <: HList,
        HL <: Nat,
        T <: HList,
        TL <: Nat](
        kvp: KvpHListCollectionHead[String, DefaultValues, HO, NO, H, HL, T, TL]): String => HO =
        ???

      override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
        kvp: KvpSingleValueHead[String, DefaultValues, H, T, TL, O]): String => O = ???

      override def kvpCoproduct[C <: Coproduct](
        value: KvpCoproduct[String, DefaultValues, C]): String => C = ???
    }

  }

}
