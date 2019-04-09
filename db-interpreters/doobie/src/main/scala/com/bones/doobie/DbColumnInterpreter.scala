package com.bones.doobie

import com.bones.data.Value._
import shapeless.{HNil,HList,Nat}

/** Responsible for getting a list of columns for the select or insert clause */
object DbColumnInterpreter {

  type ColumnName = String
  type ToColumns = ColumnName => List[String]


  private def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): List[String] = {
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
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  private val nameToColumn: ColumnName => List[String] = name => DoobieUtil.camelToSnake(name) :: Nil

  private def valueDefinition[A](fgo: ValueDefinitionOp[A]): ToColumns =
    fgo match {
      case op: OptionalValueDefinition[b] =>
          valueDefinition(op.valueDefinitionOp)
      case ob: BooleanData => nameToColumn
      case rs: StringData => nameToColumn
      case ri: LongData => nameToColumn
      case uu: UuidData => nameToColumn
      case dd: DateTimeData => nameToColumn
      case bd: BigDecimalData => nameToColumn
      case ld: ListData[t] => nameToColumn
      case ed: EitherData[a,b] => nameToColumn
      case esd: EnumerationStringData[a] => nameToColumn
      case esd: EnumStringData[a] => nameToColumn
      case kvp: KvpGroupData[h,hl] =>
        _ => kvpGroup(kvp.kvpGroup)
      case x: XMapData[a,al,b] =>
        _ => kvpGroup(x.from)
    }
}
