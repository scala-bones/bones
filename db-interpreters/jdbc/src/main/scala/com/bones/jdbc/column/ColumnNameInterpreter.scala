package com.bones.jdbc.column

import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.data.template.KvpCollectionMatch
import com.bones.jdbc.DbUtil._
import shapeless.{Coproduct, HList, Nat, ::}

/** Responsible for determining the column names to be used in the select statement */
trait ColumnNameInterpreter[ALG[_]] extends KvpCollectionMatch[String, ALG, List[ColumnName]] {

  override def kvpNil(kvp: KvpNil[String, ALG]): List[ColumnName] = List.empty

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]): List[ColumnName] = {
    fromKvpCollection(kvp.head) ::: fromKvpCollection(kvp.tail)
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]): List[ColumnName] = {
    val head = kvp.head match {
      case Left(keyDef) =>
        determineValueDefinition(keyDef.dataDefinition)(keyDef.key)
      case Right(kvpCol) =>
        fromKvpCollection(kvpCol)
    }
    val tail = fromKvpCollection(kvp.tail)
    head ::: tail
  }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[String, ALG, C]): List[ColumnName] = {
    value match {
      case _: KvpCoNil[String, _] @unchecked => List.empty
      case co: KvpCoproductCollectionHead[String, ALG, a, c, o] =>
        val head = fromKvpCollection(co.kvpCollection)
        val tail = kvpCoproduct(co.kvpTail)
        head ::: tail
    }
  }

  def generateColumnNames[A](collection: KvpCollection[String, ALG, A]): List[ColumnName] =
    fromKvpCollection(collection)

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]): List[ColumnName] =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]): List[ColumnName] =
    "dtype" :: fromKvpCollection(wrappedCoproduct.wrappedEncoding)

  type CoproductName = String

  private val keyToColumnNames: Key => List[ColumnName] = key => List(camelToSnake(key))

  def determineValueDefinition[A](
    kvp: CoproductDataDefinition[String, ALG, A]): Key => List[ColumnName] =
    kvp match {
      case Left(kvp) => valueDefinition(kvp)
      case Right(_)  => keyToColumnNames
    }

  def valueDefinition[A](fgo: HigherOrderValue[String, ALG, A]): Key => List[ColumnName] =
    fgo match {
      case op: OptionalValue[String, ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp)
      case _: ListData[String, ALG, t] @unchecked => keyToColumnNames
      case ed: EitherData[String, ALG, a, b] @unchecked =>
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
      case kvp: KvpCollectionValue[String, ALG, a] @unchecked =>
        _ =>
          fromKvpCollection(kvp.kvpCollection)
    }

}
