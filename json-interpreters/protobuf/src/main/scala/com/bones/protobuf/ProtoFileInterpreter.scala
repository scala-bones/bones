package com.bones.protobuf

import com.bones.data.Value._
import shapeless.{HList, Nat}

object ProtoFileInterpreter {

  def dataClass[A](dc: DataClass[A]): Unit = {
    dc match {
      case t: XMapData[a, al, b] => ???
      case o: OptionalDataClass[a] => ???
      case ld: XMapListData[b] => ???
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): Unit = {
    group match {
      case KvpNil => ???
      case op: KvpSingleValueHead[h, t, tl, a] => ???
      case op: KvpGroupHead[a, al, h, hl, t, tl] => ???
      case op: KvpDataClassHead[h,t,tl,out] => ???
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Unit =
    fgo match {
      case op: OptionalValueDefinition[a] => ???
      case ob: BooleanData => ???
      case rs: StringData => ???
      case ri: LongData => ???
      case uu: UuidData => ???
      case dd: DateTimeData => ???
      case bd: BigDecimalData => ???
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] => ???
      case esd: EnumStringData[a] => ???
      case kvp: KvpGroupData[h,hl] => ???
    }

}
