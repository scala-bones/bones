package com.bones.jdbc.column

import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.data.values.CNilF
import com.bones.jdbc.DbUtil
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{:+:, Coproduct, HList, Inl, Inr, Nat}

/** Responsible for getting a list of columns for the select or insert clause */
object DbColumnInterpreter {

  case class Column(name: String, columnDefinition: String, nullable: Boolean)
  case class Table(name: String, columns: List[Column])

  type Key = String
  type ToColumns = Key => List[Column]

  def tableDefinitionCustomAlgebra[ALG[_], A](
    collection: ConcreteValue[ALG, A],
    columnInterpreter: ColumnValue[ALG]): String = {
    def nullableString(nullable: Boolean) = if (nullable) "" else " not null"
    val result = valueDefinition(collection, columnInterpreter)("")
    val tableName = camelToSnake(collection.manifestOfA.runtimeClass.getSimpleName)
    val columnsWithId = Column("id", "SERIAL", false) :: result
    val columnString = columnsWithId
      .map(c => s"${c.name} ${c.columnDefinition}${nullableString(c.nullable)}")
      .mkString("(", ", ", ")")
    s"create table $tableName $columnString"
  }

  private def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpCollection[ALG, H, HL],
    customInterpreter: ColumnValue[ALG]): List[Column] = {
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headResult =
          determineValueDefinition(op.fieldDefinition.dataDefinition, customInterpreter)(
            op.fieldDefinition.key)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
      case op: KvpCollectionHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headResult = kvpHList(op.head, customInterpreter)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
      case op: KvpConcreteValueHead[ALG, a, ht, nt] =>
        val headResult = generateColumns(op.collection, customInterpreter)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
    }
  }

  private def generateColumns[ALG[_], A](
    collection: ConcreteValue[ALG, A],
    customInterpreter: ColumnValue[ALG]): List[Column] =
    valueDefinition(collection, customInterpreter)("")

  def nameToColumn(columnDefinition: String): ToColumns =
    name => List(Column(DbUtil.camelToSnake(name), columnDefinition, false))

  private def determineValueDefinition[ALG[_], A](
    coDef: CoproductDataDefinition[ALG, A],
    customInterpreter: ColumnValue[ALG]
  ): ToColumns = {
    coDef match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.toColumns(alg)
    }
  }

  private def valueDefinition[ALG[_], A](
    fgo: ConcreteValue[ALG, A],
    customInterpreter: ColumnValue[ALG]): ToColumns =
    fgo match {
      case op: OptionalValue[ALG, b] @unchecked =>
        key =>
          determineValueDefinition(op.valueDefinitionOp, customInterpreter)(key)
            .map(_.copy(nullable = true))
      case ld: ListData[ALG, t] @unchecked => ???
      case ed: EitherData[ALG, a, b] @unchecked =>
        name =>
          {
            determineValueDefinition(ed.definitionA, customInterpreter)(name) ::: determineValueDefinition(
              ed.definitionB,
              customInterpreter)(name)
          }
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        _ =>
          kvpHList(kvp.kvpHList, customInterpreter)
      case x: SwitchEncoding[ALG, a, al, b] @unchecked =>
        _ =>
          kvpHList(x.from, customInterpreter)
      case co: CoproductSwitch[ALG, c, a] @unchecked  => ??? // TODO
      case co: CoproductCollection[ALG, c] @unchecked => ??? // TODO

    }
}
