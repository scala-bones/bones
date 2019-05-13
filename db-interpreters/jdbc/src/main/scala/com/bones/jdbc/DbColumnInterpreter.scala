package com.bones.jdbc

import com.bones.data.Value._
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{HList, Nat}

/** Responsible for getting a list of columns for the select or insert clause */
object DbColumnInterpreter {

  case class Column(name: String, columnDefinition: String, nullable: Boolean)
  case class Table(name: String, columns: List[Column])

  type Key = String
  type ToColumns = Key => List[Column]

  def tableDefinition[A](bonesSchema: BonesSchema[A]): String = {
    def nullableString(nullable: Boolean) = if (nullable) "" else " not null"
    bonesSchema match {
      case x: HListConvert[h, n, b] =>
        val result = valueDefinition(x)("")
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val columnsWithId = Column("id", "SERIAL", false) :: result
        val columnString = columnsWithId
          .map(c =>
            s"${c.name} ${c.columnDefinition}${nullableString(c.nullable)}")
          .mkString("(", ", ", ")")
        s"create table $tableName $columnString"
    }
  }

  private def kvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): List[Column] = {
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        val headResult =
          valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailResult = kvpHList(op.tail)
        headResult ::: tailResult
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        val headResult = kvpHList(op.head)
        val tailResult = kvpHList(op.tail)
        headResult ::: tailResult
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] =>
        val headResult = kvpHList(op.hListConvert.from)
        val tailResult = kvpHList(op.tail)
        headResult ::: tailResult
    }
  }

  private def nameToColumn[A](columnDefinition: String): ToColumns =
    name => List(Column(DbUtil.camelToSnake(name), columnDefinition, false))

  private def valueDefinition[A](fgo: KvpValue[A]): ToColumns =
    fgo match {
      case op: OptionalKvpValueDefinition[b] =>
        key =>
          valueDefinition(op.valueDefinitionOp)(key)
            .map(_.copy(nullable = true))
      case ob: BooleanData               => nameToColumn("bool")
      case rs: StringData                => nameToColumn("text")
      case i:  IntData                   => nameToColumn("integer")
      case ri: LongData                  => nameToColumn("int8")
      case uu: UuidData                  => nameToColumn("text")
      case dd: DateTimeData              => nameToColumn("timestamp")
      case fd: FloatData                 => nameToColumn("real")
      case dd: DoubleData                => nameToColumn("double precision")
      case bd: BigDecimalData            => nameToColumn("numeric")
      case bd: ByteArrayData             => nameToColumn("bytea")
      case ld: ListData[t]               => ???
      case ed: EitherData[a, b]          =>
        name => {
          valueDefinition(ed.definitionA)(name) ::: valueDefinition(ed.definitionB)(name)
        }
      case esd: EnumerationStringData[a] => nameToColumn("text")
      case kvp: KvpHListValue[h, hl] =>
        _ =>
          kvpHList(kvp.kvpHList)
      case x: HListConvert[a, al, b] =>
        _ =>
          kvpHList(x.from)
      case m: SumTypeData[a, b] =>
        key =>
          valueDefinition(m.from)(key)
    }
}