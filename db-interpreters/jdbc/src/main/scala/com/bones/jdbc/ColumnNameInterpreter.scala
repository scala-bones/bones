package com.bones.jdbc

import com.bones.data.Value._
import com.bones.jdbc.DbUtil._
import shapeless.{HList, Nat}

/** Responsible for determining the column names to be used in the select statement */
object ColumnNameInterpreter {

  type ColumnName = String
  type Key = String

  def kvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): List[ColumnName] = {
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        val headList =
          valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailList = kvpHList(op.tail)
        headList ::: tailList
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        kvpHList(op.head) ::: kvpHList(op.tail)
      case op: KvpXMapDataHead[a, ht, nt, ho, xl, xll] =>
        valueDefinition(op.xmapData)("")
    }
  }

  private val keyToColumNames: Key => List[ColumnName] = key =>
    List(camelToSnake(key))

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Key => List[ColumnName] =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        valueDefinition(op.valueDefinitionOp)
      case ob: BooleanData               => keyToColumNames
      case rs: StringData                => keyToColumNames
      case ri: LongData                  => keyToColumNames
      case uu: UuidData                  => keyToColumNames
      case dd: DateTimeData              => keyToColumNames
      case bd: BigDecimalData            => keyToColumNames
      case ld: ListData[t]               => keyToColumNames
      case ba: ByteArrayData             => keyToColumNames
      case ed: EitherData[a, b]          => ???
      case esd: EnumerationStringData[a] => keyToColumNames
      case kvp: KvpHListValue[h, hl] =>
        _ =>
          kvpHList(kvp.kvpHList)
      case x: HListConvert[a, al, b] =>
        _ =>
          kvpHList(x.from)
      case s: SumTypeData[a, b] =>
        valueDefinition(s.from)
    }

}
