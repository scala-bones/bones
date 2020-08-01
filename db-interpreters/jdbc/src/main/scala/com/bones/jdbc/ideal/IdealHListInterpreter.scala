package com.bones.jdbc.ideal

import com.bones.data.values.InvalidStructureError
import com.bones.data._
import com.bones.jdbc.DbUtil
import shapeless.{HList, Nat, ::}

trait IdealHListInterpreter[ALG[_]]
    extends KvpHListTemplate[ALG, TableCollection => Either[InvalidStructureError, TableCollection]] {

  def fromCollection[A: Manifest](kvpCollection: KvpCollection[ALG, A]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection]

  val algInterpreter: IdealValue[ALG]


  override def kvpCollectionHead[H <: HList, HT <: HList, NT <: Nat](
    kvp: KvpCollectionHead[ALG, H, HT, NT])
    : TableCollection => Either[InvalidStructureError, TableCollection] = {
    implicit val manifestOfH = kvp.manifestOfA
    val head = fromCollection[H](kvp.collection)
    val tail = this.fromKvpHList(kvp.tail)

    (tableCollection: TableCollection) =>
      {
        val headTableCollection = head(
          tableCollection,
          Some(DbUtil.camelToSnake(kvp.manifestOfA.getClass.getSimpleName)),
          None)
        val tailTableCollection = tail(tableCollection)
        for {
          h <- headTableCollection
          t <- tailTableCollection
        } yield t.prepend(h)
      }

  }

  override def kvpHListHead[HO<:HList, NO<:Nat, H<:HList, HL<:Nat, T<:HList, TL<:Nat](
    kvp: KvpHListHead[ALG, HO, NO, H, HL, T, TL])
    : TableCollection => Either[InvalidStructureError, TableCollection] = {
    val head = fromKvpHList[H, HL](kvp.head)
    val tail = fromKvpHList[T, TL](kvp.tail)
    (tableCollection: TableCollection) =>
      {
        val headTableCollection = head(tableCollection)
        val tailTableCollection = tail(tableCollection)
        for {
          h <- headTableCollection
          t <- tailTableCollection
        } yield t.prepend(h)
      }
  }

  override def kvpNil(
    kvp: KvpNil[ALG]): TableCollection => Either[InvalidStructureError, TableCollection] =
    (tc) => Right(tc)

  override def kvpSingleValueHead[H:Manifest, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O])
    : TableCollection => Either[InvalidStructureError, TableCollection] = {
    val headF = kvp.fieldDefinition.dataDefinition match {
      case Left(kvpCollection) =>
        (tableCollection: TableCollection) =>
          {
              fromCollection[H](kvpCollection)
              .apply(
                tableCollection,
                Some(kvp.fieldDefinition.key),
                kvp.fieldDefinition.description)
          }
      case Right(alg) =>
        (tableCollection: TableCollection) =>
          {
            val newTableCollection = algInterpreter
              .columns(alg)
              .apply(tableCollection, kvp.fieldDefinition.key, kvp.fieldDefinition.description)
            Right(newTableCollection)
          }
    }

    (tableCollection: TableCollection) => for {
      h <- headF(tableCollection)
      t <- fromKvpHList(kvp.tail).apply(h)
    } yield t

  }

}
