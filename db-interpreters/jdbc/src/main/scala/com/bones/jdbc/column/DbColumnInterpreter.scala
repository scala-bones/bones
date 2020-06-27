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

  object ColumnInterpreter {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
      li: ColumnInterpreter[L],
      ri: ColumnInterpreter[R]
    ): ColumnInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new ColumnInterpreter[Lambda[A => L[A] :+: R[A]]] {

        override def toColumns[A](alg: L[A] :+: R[A]): ToColumns = alg match {
          case Inl(l) => li.toColumns(l)
          case Inr(r) => ri.toColumns(r)
        }
      }

    implicit class InterpreterOps[ALG[_], OUT](val base: ColumnInterpreter[ALG]) extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: ColumnInterpreter[R]
      ): ColumnInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)

    }

    object CNilColumnInterpreter extends ColumnInterpreter[CNilF] {
      override def toColumns[A](alg: CNilF[A]): ToColumns = sys.error("Unreachable code")
    }
  }

  trait ColumnInterpreter[ALG[_]] {
    def toColumns[A](alg: ALG[A]): ToColumns
  }

  def tableDefinitionCustomAlgebra[ALG[_], A](
    collection: KvpCollection[ALG, A],
    columnInterpreter: ColumnInterpreter[ALG]): String = {
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
    group: KvpHList[ALG, H, HL],
    customInterpreter: ColumnInterpreter[ALG]): List[Column] = {
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headResult =
          determineValueDefinition(op.fieldDefinition.dataDefinition, customInterpreter)(
            op.fieldDefinition.key)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headResult = kvpHList(op.head, customInterpreter)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] =>
        val headResult = generateColumns(op.collection, customInterpreter)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
    }
  }

  private def generateColumns[ALG[_], A](
    collection: KvpCollection[ALG, A],
    customInterpreter: ColumnInterpreter[ALG]): List[Column] =
    valueDefinition(collection, customInterpreter)("")

  def nameToColumn(columnDefinition: String): ToColumns =
    name => List(Column(DbUtil.camelToSnake(name), columnDefinition, false))

  private def determineValueDefinition[ALG[_], A](
    coDef: CoproductDataDefinition[ALG, A],
    customInterpreter: ColumnInterpreter[ALG]
  ): ToColumns = {
    coDef match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.toColumns(alg)
    }
  }

  private def valueDefinition[ALG[_], A](
    fgo: KvpCollection[ALG, A],
    customInterpreter: ColumnInterpreter[ALG]): ToColumns =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, b] @unchecked =>
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
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        _ =>
          kvpHList(x.from, customInterpreter)
    }
}
