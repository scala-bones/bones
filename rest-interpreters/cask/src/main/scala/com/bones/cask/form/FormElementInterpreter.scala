package com.bones.cask.form

import com.bones.data.{
  KvpCoproduct,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList
}
import com.bones.data.template.KvpCollectionMatch
import scalatags.Text
import shapeless.{Coproduct, HList, Nat}

trait FormElementInterpreter[ALG[_]]
    extends KvpCollectionMatch[String, ALG, List[Text.TypedTag[String]]] {

  def formElement: FormElement[ALG]

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]): List[Text.TypedTag[String]] =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]): List[Text.TypedTag[String]] =
    fromKvpCollection(wrappedCoproduct.wrappedEncoding)

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](
    kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]): List[Text.TypedTag[String]] =
    fromKvpCollection(kvp.head) ++ fromKvpCollection(kvp.tail)

  override def kvpNil(kvp: KvpNil[String, ALG]): List[Text.TypedTag[String]] = List.empty

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]): List[Text.TypedTag[String]] = {
    kvp.head match {
      case Left(keyDef) =>
        keyDef.dataDefinition match {
          case Left(hov)  => ???
          case Right(alg) => formElement.generateFormElement(alg, List.empty)
        }
      case Right(kvp) => fromKvpCollection(kvp)
    }
  }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[String, ALG, C]): List[Text.TypedTag[String]] = ???
}
