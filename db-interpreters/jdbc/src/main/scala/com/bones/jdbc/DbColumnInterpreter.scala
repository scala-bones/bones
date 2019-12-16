package com.bones.jdbc

import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data.{KeyValueDefinition, _}
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{HList, Inl, Inr, Nat}
import com.bones.syntax.NoAlgebra

/** Responsible for getting a list of columns for the select or insert clause */
object DbColumnInterpreter {

  case class Column(name: String, columnDefinition: String, nullable: Boolean)
  case class Table(name: String, columns: List[Column])

  type Key = String
  type ToColumns = Key => List[Column]

  trait CustomInterpreter[ALG[A]] {
    def toColumns[A](alg: ALG[A]): ToColumns
  }

  object NoAlgebraCustomInterpreter extends CustomInterpreter[NoAlgebra] {
    def toColumns[A](alg: NoAlgebra[A]): ToColumns = sys.error("Unreachable code")
  }

  def tableDefinition[A](bonesSchema: BonesSchema[NoAlgebra,A]): String =
    tableDefinitionCustomAlgebra(bonesSchema, NoAlgebraCustomInterpreter)

  def tableDefinitionCustomAlgebra[ALG[_], A](bonesSchema: BonesSchema[ALG,A], customInterpreter: CustomInterpreter[ALG]): String = {
    def nullableString(nullable: Boolean) = if (nullable) "" else " not null"
    bonesSchema match {
      case x: HListConvert[ALG, h, n, b] @unchecked =>
        val result = valueDefinition(x, customInterpreter)("")
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val columnsWithId = Column("id", "SERIAL", false) :: result
        val columnString = columnsWithId
          .map(c =>
            s"${c.name} ${c.columnDefinition}${nullableString(c.nullable)}")
          .mkString("(", ", ", ")")
        s"create table $tableName $columnString"
    }
  }

  private def kvpHList[ALG[_], H <: HList, HL <: Nat](
      group: KvpHList[ALG, H, HL], customInterpreter: CustomInterpreter[ALG]): List[Column] = {
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headResult =
          determineValueDefinition(op.fieldDefinition.op, customInterpreter)(op.fieldDefinition.key)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val headResult = kvpHList(op.head, customInterpreter)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] =>
        val headResult = bonesSchema(op.bonesSchema, customInterpreter)
        val tailResult = kvpHList(op.tail, customInterpreter)
        headResult ::: tailResult
    }
  }

  private def bonesSchema[ALG[_], A](bonesSchema: BonesSchema[ALG, A], customInterpreter: CustomInterpreter[ALG]): List[Column] =
    bonesSchema match {
      case co: KvpCoproductConvert[ALG, c, a] @unchecked => valueDefinition(co, customInterpreter)("")
      case co: HListConvert[ALG, h, n, a] @unchecked => valueDefinition(co, customInterpreter)("")
    }

  private def nameToColumn[A](columnDefinition: String): ToColumns =
    name => List(Column(DbUtil.camelToSnake(name), columnDefinition, false))

  private def determineValueDefinition[ALG[_], A]
    (
      coDef: CoproductDataDefinition[ALG, A],
      customInterpreter: CustomInterpreter[ALG]
    ) : ToColumns = {
      coDef match {
        case Left(kvp) => valueDefinition(kvp, customInterpreter)
        case Right(alg) => customInterpreter.toColumns(alg)
      }
    }

  private def valueDefinition[ALG[_], A](fgo: KvpValue[A], customInterpreter: CustomInterpreter[ALG]): ToColumns =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, b] @unchecked =>
        key =>
          determineValueDefinition(op.valueDefinitionOp, customInterpreter)(key)
            .map(_.copy(nullable = true))
      case ob: BooleanData               => nameToColumn("bool")
      case rs: StringData                => nameToColumn("text")
      case i:  ShortData                   => nameToColumn("int2")
      case i:  IntData                   => nameToColumn("integer")
      case ri: LongData                  => nameToColumn("int8")
      case uu: UuidData                  => nameToColumn("text")
      case dd: LocalDateData              => nameToColumn("date")
      case dd: LocalDateTimeData              => nameToColumn("timestamp")
      case fd: FloatData                 => nameToColumn("real")
      case dd: DoubleData                => nameToColumn("double precision")
      case bd: BigDecimalData            => nameToColumn("numeric")
      case bd: ByteArrayData             => nameToColumn("bytea")
      case ld: ListData[ALG,t] @unchecked  => ???
      case ed: EitherData[ALG,a, b] @unchecked =>
        name => {
          determineValueDefinition(ed.definitionA, customInterpreter)(name) ::: determineValueDefinition(ed.definitionB, customInterpreter)(name)
        }
      case esd: EnumerationData[e,a] => nameToColumn("text")
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        _ => kvpHList(kvp.kvpHList, customInterpreter)
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        _ => kvpHList(x.from, customInterpreter)
    }
}
