package com.bones.interpreter

import com.bones.data.Value._

object SqlInterpreter {

  def apply[A](fgo: ValueDefinitionOp[A]): String =
    fgo match {
      case op: OptionalValueDefinition[a] => apply(op.valueDefinitionOp) + " ! is not null"
      case KvpNil => ""
      case op: KvpGroupHead[A, al, h, hl, t, tl] => s"${apply(op.head)}, ${apply(op.tail)}"
      case op: KvpSingleValueHead[h, t, tl, A, al] => s"${op.fieldDefinition.key} ${apply(op.fieldDefinition.op)}"
      case ob: BooleanData => "boolean"
      case rs: StringData => "text"
      case ri: IntData => "int8"
      case uu: UuidData => "varchar(36)"
      case dd: DateData => "timestamp"
      case bd: BigDecimalFromString => "number"
      case dd: DoubleData => "number"
      case ld: ListData[t, l] => "list"
      case ed: EitherData[a,b] => s"${apply(ed.definitionA)} can be null , ${apply(ed.definitionB)} can be null"
      case cd: SumTypeData[a,b] => apply(cd.from)
      case esd: EnumerationStringData[a] => "text"
      case esd: EnumStringData[a] => "text"
      case br: ByteReferenceData => "byte"
      case t: Transform[a,b] => apply(t.op)
    }

}
