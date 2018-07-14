package com.bones.interpreter


import com.bones.data.Algebra._
import com.bones.data.HListAlgebra.{HDataDefinition, HListPrependN, HMember}

/**
  * Just a template to be used as a starting point for a new interpreter.
  */
class InterpreterTemplate {

  def apply[A](fgo: DataDefinitionOp[A]): Unit =
    fgo match {
      case op: OptionalDataDefinition[a] => ???
      case op: HListPrependN[A,p,s] => ???
      case op: HMember[a] => ???
      case op: HDataDefinition[a] => ???
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
    }

}
