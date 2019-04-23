package com.bones.jdbc

import com.bones.data.Value._
import DbUtil.camelToSnake
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
      case x: XMapData[h,n,b] =>
        val result = valueDefinition(x)("")
        val tableName = camelToSnake(x.manifestOfA.runtimeClass.getSimpleName)
        val columnsWithId = Column("id", "SERIAL", false) :: result
        val columnString = columnsWithId.map(c => s"${c.name} ${c.columnDefinition}${nullableString(c.nullable)}").mkString("(",", ",")")
        s"create table $tableName $columnString"
    }
  }


  private def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): List[Column] = {
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        val headResult = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailResult = kvpGroup(op.tail)
        headResult ::: tailResult
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        val headResult = kvpGroup(op.head)
        val tailResult = kvpGroup(op.tail)
        headResult ::: tailResult
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] =>
        val headResult = kvpGroup(op.xmapData.from)
        val tailResult = kvpGroup(op.tail)
        headResult ::: tailResult
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  private def nameToColumn[A](columnDefinition: String): ToColumns =
    name => List( Column(DbUtil.camelToSnake(name), columnDefinition, false) )

  private def valueDefinition[A](fgo: ValueDefinitionOp[A]): ToColumns =
    fgo match {
      case op: OptionalValueDefinition[b] =>
          key => valueDefinition(op.valueDefinitionOp)(key).map(_.copy(nullable = true))
      case ob: BooleanData => nameToColumn("bool")
      case rs: StringData => nameToColumn("text")
      case ri: LongData => nameToColumn("int8")
      case uu: UuidData => nameToColumn("text")
      case dd: DateTimeData => nameToColumn("timestamp")
      case bd: BigDecimalData => nameToColumn("numeric")
      case bd: ByteArrayData => nameToColumn("bytea")
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] => nameToColumn("text")
      case esd: EnumStringData[a] => nameToColumn("text")
      case kvp: KvpGroupData[h,hl] =>
        _ => kvpGroup(kvp.kvpGroup)
      case x: XMapData[a,al,b] =>
        _ => kvpGroup(x.from)
      case m: SumTypeData[a,b] =>
        key => valueDefinition(m.from)(key)
    }
}
