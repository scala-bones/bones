package com.bones.interpreter

import com.bones.data.Algebra._
import com.bones.data.HListAlgebra.{HDataDefinition, HListPrependN, HMember}

object SqlInterpreter {

  def apply[A](fgo: DataDefinitionOp[A]): String =
    fgo match {
      case op: OptionalDataDefinition[a] => apply(op.dataDefinitionOp) + " ! is not null"
      case op: HListPrependN[A,p,s] => s"${apply(op.prefix)}, ${apply(op.suffix)}"
      case op: HMember[a] => s"${op.op1.key.name} ${apply(op.op1.op)}"
      case op: HDataDefinition[a] => apply(op.op)
      case ob: BooleanData => "boolean"
      case rs: StringData => "text"
      case ri: IntData => "int8"
      case uu: UuidData => "varchar(36)"
      case dd: DateData => "timestamp"
      case bd: BigDecimalFromString => "number"
      case dd: DoubleData => "number"
      case ld: ListData[t, l] => "list"
      case ed: EitherData[a,b] => s"${apply(ed.definitionA)} can be null , ${apply(ed.definitionB)} can be null"
      case cd: ConversionData[a,b] => apply(cd.from)
      case esd: EnumerationStringData[a] => "text"
      case esd: EnumStringData[a] => "text"
      case t: Transform[a,b] => apply(t.op)
    }

}
