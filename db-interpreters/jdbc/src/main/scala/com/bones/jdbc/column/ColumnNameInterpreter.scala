package com.bones.jdbc.column

import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.jdbc.DbUtil._
import shapeless.{Coproduct, HList, Nat}

/** Responsible for determining the column names to be used in the select statement */
object ColumnNameInterpreter {

  type ColumnName = String
  type Key = String

  def kvpHList[ALG[_], H <: HList, HL <: Nat](group: KvpHList[ALG, H, HL]): List[ColumnName] = {
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headList =
          determineValueDefinition(op.fieldDefinition.dataDefinition)(op.fieldDefinition.key)
        val tailList = kvpHList(op.tail)
        headList ::: tailList
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        kvpHList(op.head) ::: kvpHList(op.tail)
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] =>
        fromBonesSchema(op.bonesSchema)
    }
  }

  def fromBonesSchema[ALG[_], A](bonesSchema: KvpCollection[ALG, A]): List[ColumnName] =
    bonesSchema match {
      case hList: HListConvert[ALG, h, n, a] @unchecked => kvpHList(hList.from)
      case co: KvpCoproductConvert[ALG, c, a] @unchecked =>
        valueDefinition(co)("")
    }

  type CoproductName = String

  protected def kvpCoproduct[ALG[_], C <: Coproduct](
    kvpCo: KvpCoproduct[ALG, C]): List[ColumnName] =
    kvpCo match {
      case co: KvpCoNil[_] => List.empty
      case co: KvpSingleValueLeft[ALG, l, r] =>
        val head = determineValueDefinition(co.kvpValue)("")
        val tail = kvpCoproduct(co.kvpTail)
        head ::: tail
    }

  private val keyToColumnNames: Key => List[ColumnName] = key => List(camelToSnake(key))

  def determineValueDefinition[ALG[_], A](
    kvp: CoproductDataDefinition[ALG, A]): Key => List[ColumnName] =
    kvp match {
      case Left(kvp) => valueDefinition(kvp)
      case Right(_)  => keyToColumnNames
    }

  def valueDefinition[ALG[_], A](fgo: KvpCollection[ALG,A]): Key => List[ColumnName] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp)
      case ld: ListData[ALG, t] @unchecked => keyToColumnNames
      case ed: EitherData[ALG, a, b] @unchecked =>
        key =>
          {
            val baseName = camelToSnake(key)
            List("left_" + baseName, "right_" + baseName)
          }
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        _ =>
          kvpHList(kvp.kvpHList)
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        _ =>
          kvpHList(x.from)
      case x: KvpCoproductConvert[ALG, c, a] @unchecked =>
        _ =>
          {
            val columnNames = kvpCoproduct(x.from)
            "dtype" :: columnNames
          }
    }

}