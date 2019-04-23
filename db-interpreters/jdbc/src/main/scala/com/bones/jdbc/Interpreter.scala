package com.bones.jdbc

import java.util.{Calendar, TimeZone}

import com.bones.data.Value._
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{HList, Nat}

object FindInterpreter {

  val utcCalendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
  type FieldName = String
  type Path = List[String]

}

object TableName {
  def getTableName[B](dc: BonesSchema[B]): String = dc match {
    case t: XMapData[a, al, b] => camelToSnake(t.manifestOfA.runtimeClass.getSimpleName)
  }
}



object FieldNames {

  def fromSchema[A](dc: BonesSchema[A]): List[String] =
    dc match {
      case t: XMapData[a, al, b] => kvpGroup(t.from)
    }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): List[String] =
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] => List(camelToSnake(op.fieldDefinition.key)) ::: kvpGroup(op.tail)
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] =>
        kvpGroup(op.xmapData.from) ::: kvpGroup(op.tail)
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        kvpGroup(op.head) ::: kvpGroup(op.tail)
      case op: OptionalKvpGroup[h,hl] =>
        kvpGroup(op.kvpGroup)
    }


  def valueDefinition[A](fgo: ValueDefinitionOp[A]): List[String] =
    fgo match {
      case op: OptionalValueDefinition[a] => valueDefinition(op.valueDefinitionOp)
      case ob: BooleanData => List.empty
      case rs: StringData => List.empty
      case ri: LongData => List.empty
      case uu: UuidData => List.empty
      case dd: DateTimeData => List.empty
      case bd: BigDecimalData => List.empty
      case ba: ByteArrayData => List.empty
      case ld: ListData[t] => List.empty
      case ed: EitherData[a,b] => List.empty
      case esd: EnumerationStringData[a] => List.empty
      case esd: EnumStringData[a] => List.empty
      case st: SumTypeData[a,b] => valueDefinition(st.from)
      case kvp: KvpGroupData[h,hl] => kvpGroup(kvp.kvpGroup)
      case x: XMapData[_,_,_] => kvpGroup(x.from)
    }

}

