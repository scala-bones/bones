package com.bones.react

import com.bones.data.Value._
import shapeless.{HList, Nat}
import scalatags.Text.all._

object ValidationInterpreter {

}
object FormInterpreter {

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): Unit = {
    group match {
      case KvpNil => ???
      case op: KvpSingleValueHead[h, t, tl, a] => ???
      case op: KvpGroupHead[a, al, h, hl, t, tl] => ???
      case op: OptionalKvpGroup[h,hl] => ???
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] => ???
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Unit =
    fgo match {
      case op: OptionalValueDefinition[a] => ???
      case ob: BooleanData => input()
      case rs: StringData => ???
      case ri: LongData => ???
      case uu: UuidData => ???
      case dd: DateTimeData => ???
      case bd: BigDecimalData => ???
      case ba: ByteArrayData => ???
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] => ???
      case esd: EnumStringData[a] => ???
      case kvp: KvpGroupData[h,hl] => ???
      case t: XMapData[a, al, b] => ???
      case s: SumTypeData[a,b] => ???

    }
}
