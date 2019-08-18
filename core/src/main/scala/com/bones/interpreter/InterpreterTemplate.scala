package com.bones.interpreter

import com.bones.data.Value._
import shapeless.{HList, Nat}

/**
  * Just a template to be used as a starting point for a new interpreter.
  * You can copy/paste this for a starting point for a new interpreter.
  */
class InterpreterTemplate {

  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL]): Unit = {
    group match {
      case KvpNil => ???
      case op: KvpSingleValueHead[h, t, tl, a] => ???
      case op: KvpHListHead[a, al, h, hl, t, tl] => ???
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] => ???
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): Unit =
    fgo match {
      case op: OptionalKvpValueDefinition[a] => ???
      case ob: BooleanData => ???
      case rs: StringData => ???
      case id: IntData => ???
      case ri: LongData => ???
      case uu: UuidData => ???
      case dd: LocalDateTimeData => ???
      case ld: LocalDateData => ???
      case fd: FloatData => ???
      case sd: ShortData => ???
      case id: DoubleData => ???
      case bd: BigDecimalData => ???
      case ld: ListData[t] => ???
      case ed: EitherData[a, b] => ???
      case ba: ByteArrayData => ???
      case esd: EnumerationData[a,b] => ???
      case kvp: KvpHListValue[h, hl] => ???
      case x: HListConvert[a, al, b] => ???
      case s: SumTypeData[a, b] => ???
    }

}
