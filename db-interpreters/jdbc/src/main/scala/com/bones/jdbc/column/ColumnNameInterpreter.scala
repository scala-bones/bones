package com.bones.jdbc.column

import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.data.template.KvpCollectionMatch
import com.bones.jdbc.DbUtil._
import shapeless.{Coproduct, HList, Nat, ::}

/** Responsible for determining the column names to be used in the select statement */
trait ColumnNameInterpreter[ALG[_]] extends KvpCollectionMatch[ALG, List[ColumnName]] {

  override def kvpNil(kvp: KvpNil[ALG]): List[ColumnName] = List.empty

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): List[ColumnName] = {
    fromKvpCollection(kvp.head) ::: fromKvpCollection(kvp.tail)
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): List[ColumnName] = {
    val head = kvp.head match {
      case Left(keyDef) =>
        determineValueDefinition(keyDef.dataDefinition)(keyDef.key)
      case Right(kvpCol) =>
        fromKvpCollection(kvpCol)
    }
    val tail = fromKvpCollection(kvp.tail)
    head ::: tail
  }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[ALG, C]): List[ColumnName] = {
    value match {
      case _: KvpCoNil[_] => List.empty
      case co: KvpCoproductCollectionHead[ALG, a, c, o] =>
        val head = fromKvpCollection(co.kvpCollection)
        val tail = kvpCoproduct(co.kvpTail)
        head ::: tail
    }
  }

  def generateColumnNames[A](collection: KvpCollection[ALG, A]): List[ColumnName] =
    fromKvpCollection(collection)

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL]): List[ColumnName] =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): List[ColumnName] =
    "dtype" :: fromKvpCollection(wrappedCoproduct.wrappedEncoding)

  type CoproductName = String

  private val keyToColumnNames: Key => List[ColumnName] = key => List(camelToSnake(key))

  def determineValueDefinition[A](kvp: CoproductDataDefinition[ALG, A]): Key => List[ColumnName] =
    kvp match {
      case Left(kvp) => valueDefinition(kvp)
      case Right(_)  => keyToColumnNames
    }

  def valueDefinition[A](fgo: HigherOrderValue[ALG, A]): Key => List[ColumnName] =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp)
      case _: ListData[ALG, t] @unchecked => keyToColumnNames
      case ed: EitherData[ALG, a, b] @unchecked =>
        key =>
          {
            val leftColumns = determineValueDefinition(ed.definitionA)(key)
            val rightColumns = determineValueDefinition(ed.definitionB)(key)

            //If there are duplicate names, then we will append the name of the type
            val (matchingLeft, uniqueLeft) =
              leftColumns.partition(colA => rightColumns.contains(colA))
            val (matchingRight, uniqueRight) =
              rightColumns.partition(colB => leftColumns.contains(colB))

            matchingLeft.map(_ + "_" + camelToSnake(ed.typeNameOfA)) :::
              uniqueLeft :::
              matchingRight.map(_ + "_" + camelToSnake(ed.typeNameOfB)) :::
              uniqueRight :::
              Nil
          }
      case kvp: KvpCollectionValue[ALG, a] @unchecked =>
        _ =>
          fromKvpCollection(kvp.kvpCollection)
    }

}
