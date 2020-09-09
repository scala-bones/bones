package com.bones.jdbc.column

import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data.KvpCollection.headManifest
import com.bones.data._
import com.bones.data.template.KvpCollectionMatch
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{::, Coproduct, HList, Nat}

/** Responsible for getting a list of columns for the select or insert clause */
trait DbColumnInterpreter[ALG[_]] extends KvpCollectionMatch[ALG, List[Column]] {

  def customInterpreter: ColumnValue[ALG]

  def tableDefinitionCustomAlgebra[A](collection: KvpCollection[ALG, A]): String = {
    def nullableString(nullable: Boolean) = if (nullable) "" else " not null"
    val result = fromKvpCollection(collection)
    val tableName = camelToSnake(
      headManifest(collection).map(_.runtimeClass.getSimpleName).getOrElse("unknown"))
    val columnsWithId = Column("id", "SERIAL", false) :: result
    val columnString = columnsWithId
      .map(c => s"${c.name} ${c.columnDefinition}${nullableString(c.nullable)}")
      .mkString("(", ", ", ")")
    s"create table $tableName $columnString"
  }

  override def kvpNil(kvp: KvpNil[ALG]): List[Column] = List.empty

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL]): List[Column] =
    fromKvpCollection(wrappedHList.wrappedEncoding)

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): List[Column] =
    Column("dtype", "Used to specify the specific coproduct type", true) :: fromKvpCollection(
      wrappedCoproduct.wrappedEncoding)

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): List[Column] = {
    val head = fromKvpCollection(kvp.head)
    val tail = fromKvpCollection(kvp.tail)
    head ::: tail
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): List[Column] = {
    kvp.head match {
      case Left(keyDef)  => determineValueDefinition(keyDef.dataDefinition)(keyDef.key)
      case Right(kvpCol) => fromKvpCollection(kvpCol)
    }
  }

  private def determineValueDefinition[A](
    coDef: CoproductDataDefinition[ALG, A]
  ): ToColumns = {
    coDef match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(alg) => customInterpreter.toColumns(alg)
    }
  }

  private def valueDefinition[A](fgo: PrimitiveWrapperValue[ALG, A]): ToColumns =
    fgo match {
      case op: OptionalValue[ALG, b] @unchecked =>
        key =>
          determineValueDefinition(op.valueDefinitionOp)(key)
            .map(_.copy(nullable = true))
      case ld: ListData[ALG, t] @unchecked => ???
      case ed: EitherData[ALG, a, b] @unchecked =>
        name =>
          {
            determineValueDefinition(ed.definitionA)(name) ::: determineValueDefinition(
              ed.definitionB)(name)
          }
      case kvp: KvpCollectionValue[ALG, a] @unchecked =>
        _ =>
          fromKvpCollection(kvp.kvpCollection)
    }

  override def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[ALG, C]): List[Column] = {
    value match {
      case _: KvpCoNil[ALG] => List.empty
      case kvpHead: KvpCoproductCollectionHead[ALG, a, c, o] => {
        val head = fromKvpCollection(kvpHead.kvpCollection)
        val tail = kvpCoproduct(kvpHead.kvpTail)
        head ::: tail
      }
    }
  }
}
