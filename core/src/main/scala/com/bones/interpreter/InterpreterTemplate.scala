package com.bones.interpreter


import com.bones.data.Value._

/**
  * Just a template to be used as a starting point for a new interpreter.
  */
class InterpreterTemplate {

  def apply[A](fgo: ValueDefinitionOp[A]): Unit =
    fgo match {
      case op: OptionalValueDefinition[a] => ???
      case op: KvpSingleValueHead[h,t,tl,A,al] => ???
      case op: KvpGroupHead[A,al, h, hl, t, tl] => ???
      case ob: BooleanData => ???
      case rs: StringData => ???
      case ri: IntData => ???
      case uu: UuidData => ???
      case dd: DateData => ???
      case bd: BigDecimalFromString => ???
      case dd: DoubleData => ???
      case ld: ListData[t, l] => ???
      case ed: EitherData[a,b] => ???
      case cd: ConversionData[a,b] => ???
      case esd: EnumerationStringData[a] => ???
      case esd: EnumStringData[a] => ???
      case t: Transform[a,b] => ???
      case tod: ToOptionalData[a] => ???
    }

}
