package com.bones.liquibase

import com.bones.data._
import com.bones.jdbc.DbUtil
import liquibase.structure.core._
import shapeless.{Coproduct, HList, Nat}

object GenerateDatabaseObject {

  type Name = String

  case class TableItems(
    columns: List[Column],
    primaryKeys: List[PrimaryKey],
    foreignKeys: List[ForeignKey],
    uniqueConstraint: List[UniqueConstraint]
  )

  private def combine(
    l: (List[(Table, TableItems)], List[TableItems]),
    r: (List[(Table, TableItems)], List[TableItems]))
    : (List[(Table, TableItems)], List[TableItems]) =
    (l._1 ::: r._1, l._2 ::: r._2)

  def generateDatabaseObjects[ALG[_], A, B](
    collection: KvpCollection[ALG, A],
    idValue: KvpCollection[ALG, B],
    valueInterpreter: DatabaseObjectValue[ALG]): (List[(Table)], List[TableItems]) = {

    val tableName = DbUtil.camelToSnake(collection.manifestOfA.runtimeClass.getSimpleName)
    val idColumn = valueOfCollection(idValue, None, valueInterpreter)._2.flatMap(_.columns)

    val columns = valueOfCollection(collection, None, valueInterpreter)._2
    val table = new Table("catalog", "schema", tableName)
    val primaryKey = new PrimaryKey("id", "catalog", "schema", tableName, idColumn:_*)
    table.setPrimaryKey(primaryKey)
    columns.flatMap(_.columns).foreach(table.addColumn(_))

    (List(table), List.empty)
  }

  private def valueOf[ALG[_], A](
    alg: Either[KvpCollection[ALG, A], ALG[A]],
    name: String,
    value: DatabaseObjectValue[ALG]): (List[(Table, TableItems)], List[TableItems]) = {
    alg match {
      case Left(collection) => valueOfCollection(collection, Some(name), value)
      case Right(alg)       => value.databaseObject(alg)(name)
    }
  }

  private def valueOfCollection[ALG[_], A](
    fgo: KvpCollection[ALG, A],
    name: Option[String],
    valueInterpreter: DatabaseObjectValue[ALG]): (List[(Table, TableItems)], List[TableItems]) =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, b] @unchecked =>
        val items = valueOf(op.valueDefinitionOp, name.getOrElse("optional"), valueInterpreter)
        items._2.foreach(_.columns.foreach(_.setNullable(true)))
        items
      case ld: ListData[ALG, t] @unchecked => ???
      case ed: EitherData[ALG, a, b] @unchecked =>
        val left = valueOf(ed.definitionA, name.map("left_"+_).getOrElse("left"), valueInterpreter)
        val right = valueOf(ed.definitionB, name.map("right_"+_).getOrElse("right"), valueInterpreter)
        (left._1 ::: right._1, left._2 ::: right._2)
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        kvpHList(kvp.kvpHList, valueInterpreter)
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        kvpHList(x.from, valueInterpreter)
      case co: KvpCoproductConvert[ALG, c, a] @unchecked =>
        kvpCoproduct(co.from, valueInterpreter)
      case co: KvpCoproductValue[ALG,c] @unchecked =>
        kvpCoproduct(co.kvpCoproduct, valueInterpreter)
    }

  private def kvpCoproduct[ALG[_], C <: Coproduct](
    kvpCo: KvpCoproduct[ALG, C],
    dov: DatabaseObjectValue[ALG]
  ): (List[(Table, TableItems)], List[TableItems]) = {
    kvpCo match {
      case _: KvpCoNil[_] => (List.empty, List.empty)
      case co: KvpSingleValueLeft[ALG, l, r] =>
        val name = DbUtil.camelToSnake(co.manifestL.runtimeClass.getSimpleName)
        val left = valueOf(co.kvpValue, name, dov)
        val right = kvpCoproduct(co.kvpTail, dov)
        combine(left, right)
    }
  }

  private def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    valueInterpreter: DatabaseObjectValue[ALG]): (List[(Table, TableItems)], List[TableItems]) = {
    group match {
      case _: KvpNil[_] => (List.empty, List.empty)
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headResult =
          valueOf(
            op.fieldDefinition.dataDefinition,
            DbUtil.camelToSnake(op.fieldDefinition.key),
            valueInterpreter)
        val tailResult = kvpHList(op.tail, valueInterpreter)
        combine(headResult, tailResult)
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headResult = kvpHList(op.head, valueInterpreter)
        val tailResult = kvpHList(op.tail, valueInterpreter)
        combine(headResult, tailResult)
      case op: KvpCollectionHead[ALG, a, ht, nt] =>
        val headResult = valueOfCollection(op.collection, None, valueInterpreter)
        val tailResult = kvpHList(op.tail, valueInterpreter)
        //TODO combine head/tail into single table
        combine(headResult, tailResult)
    }
  }

}
