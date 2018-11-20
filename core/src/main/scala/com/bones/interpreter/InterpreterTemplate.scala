package com.bones.interpreter


import com.bones.data.Value._
import shapeless.{HList, Nat}

/**
  * Just a template to be used as a starting point for a new interpreter.
  */
class InterpreterTemplate {

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): Unit = {
    group match {
      case KvpNil => ???
      case op: KvpSingleValueHead[h, t, tl, a, al] => ???
      case op: KvpGroupHead[a, al, h, hl, t, tl] => ???
      case t: XMapData[a, al, b] => ???
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Unit =
    fgo match {
      case op: OptionalValueDefinition[a] => ???
      case ob: BooleanData => ???
      case rs: StringData => ???
      case ri: IntData => ???
      case uu: UuidData => ???
      case dd: DateData => ???
      case bd: BigDecimalFromString => ???
      case dd: DoubleData => ???
      case ld: ListData[t, l] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] => ???
      case esd: EnumStringData[a] => ???
      case tod: ToOptionalData[a] => ???
      case kvp: KvpGroupData[h,hl] => ???
      case br: ByteReferenceData => ???
    }

}
