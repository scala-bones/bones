package com.bones.jdbc.column

import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data.KvpCollection.headTypeName
import com.bones.data._
import com.bones.data.template.KvpCollectionMatch
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{::, Coproduct, HList, Nat}

/** Responsible for getting a list of columns for the select or insert clause */
trait DbColumnInterpreter[ALG[_]] extends KvpCollectionMatch[String, ALG, List[Column]] {

  def customInterpreter: ColumnValue[ALG]

  def tableDefinitionCustomAlgebra[A](collection: KvpCollection[String, ALG, A]): String = {
    def nullableString(nullable: Boolean) = if (nullable) "" else " not null"
    val result = fromKvpCollection(collection)
    val tableName = camelToSnake(headTypeName(collection).getOrElse("Unknown"))
    val columnsWithId = Column("id", "SERIAL", false) :: result
    val columnString = columnsWithId
      .map(c => s"${c.name} ${c.columnDefinition}${nullableString(c.nullable)}")
      .mkString("(", ", ", ")")
    s"create table $tableName $columnString"
  }

  override def kvpNil(kvp: KvpNil[String, ALG]): List[Column] = List.empty

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]
  ): List[Column] =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C]
  ): List[Column] =
    Column("dtype", "Used to specify the specific coproduct type", true) :: fromKvpCollection(
      wrappedCoproduct.wrappedEncoding
    )

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat
  ](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL]): List[Column] = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    head ::: tail
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]
  ): List[Column] = {
    kvp.head match {
      case Left(keyDef)  => determineValueDefinition(keyDef.dataDefinition)(keyDef.key)
      case Right(kvpCol) => fromKvpCollection(kvpCol)
    }
  }

  private def determineValueDefinition[A](
    coDef: CoproductDataDefinition[String, ALG, A]
  ): ToColumns = {
    coDef match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.toColumns(alg)
    }
  }

  private def valueDefinition[A](fgo: HigherOrderValue[String, ALG, A]): ToColumns =
    fgo match {
      case op: OptionalValue[String, ALG, b] @unchecked =>
        key =>
          determineValueDefinition(op.valueDefinitionOp)(key)
            .map(_.copy(nullable = true))
      case ld: ListData[String, ALG, t] @unchecked => ???
      case ed: EitherData[String, ALG, a, b] @unchecked =>
        name => {
          determineValueDefinition(ed.definitionA)(name) ::: determineValueDefinition(
            ed.definitionB
          )(name)
        }
      case kvp: KvpCollectionValue[String, ALG, a] @unchecked =>
        _ => fromKvpCollection(kvp.kvpCollection)
    }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[String, ALG, C]): List[Column] = {
    value match {
      case _: KvpCoNil[String, ALG] => List.empty
      case kvpHead: KvpCoproductCollectionHead[String, ALG, a, c, o] => {
        val head = fromKvpCollection(kvpHead.kvpCollection)
        val tail = kvpCoproduct(kvpHead.kvpTail)
        head ::: tail
      }
    }
  }
}
