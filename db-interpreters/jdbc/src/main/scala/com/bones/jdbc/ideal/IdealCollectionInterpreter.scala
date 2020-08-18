package com.bones.jdbc.ideal

import com.bones.data._
import shapeless.{::, HList, Nat}

trait IdealCollectionInterpreter[ALG[_]]
    extends KvpCollectionTemplate[ALG, TableCollection => TableCollection] {

  /**
    * This is used to deconstruct a ConcreteValue when it is an item
    * in the HList.  Here we do not have a key value pair, we just have
    * a collection of Key Value Pairs.
    * @param concreteValue
    * @tparam A
    * @return
    */
  def fromConcreteValue[A: Manifest](
    concreteValue: PrimitiveWrapperValue[ALG, A]): TableCollection => TableCollection

  /**
    * This is used when we have found a concrete value at the head of the HList.
    * Here we have a key value pair to deconstruct.
    * @param cv
    * @tparam A
    * @return
    */
  def fromNestedConcreteValue[A](cv: PrimitiveWrapperValue[ALG, A])
    : (TableCollection, ColumnName, Option[Description]) => TableCollection

  val algInterpreter: IdealValue[ALG]

  override def kvpConcreteValueHead[H <: HList, HT <: HList, NT <: Nat](
    kvp: KvpConcreteValueHead[ALG, H, HT, NT]): TableCollection => TableCollection = {
    implicit val manifestOfH = kvp.manifestOfA
    val head = fromConcreteValue[H](kvp.collection)
    val tail = this.fromKvpCollection(kvp.wrappedEncoding)

    (tableCollection: TableCollection) =>
      {
        val headTableCollection = head(tableCollection)
        tail(headTableCollection)
      }

  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): TableCollection => TableCollection = {
    val head = fromKvpCollection[H, HL](kvp.head)
    val tail = fromKvpCollection[T, TL](kvp.tail)
    (tableCollection: TableCollection) =>
      {
        val headTableCollection = head(tableCollection)
        tail(headTableCollection)
      }
  }

  override def kvpNil(kvp: KvpNil[ALG]): TableCollection => TableCollection =
    identity

  override def kvpSingleValueHead[H: Manifest, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): TableCollection => TableCollection = {
    val headF = kvp.fieldDefinition.dataDefinition match {
      case Left(concreteValue) =>
        (tableCollection: TableCollection) =>
          {
            fromNestedConcreteValue[H](concreteValue)
              .apply(tableCollection, kvp.fieldDefinition.key, kvp.fieldDefinition.description)
          }
      case Right(alg) =>
        (tableCollection: TableCollection) =>
          {
            val newTableCollection = algInterpreter
              .columns(alg)
              .apply(tableCollection, kvp.fieldDefinition.key, kvp.fieldDefinition.description)
            newTableCollection
          }
    }

    (tableCollection: TableCollection) =>
      {
        val h = headF(tableCollection)
        fromKvpCollection(kvp.tail).apply(h)
      }

  }

}
