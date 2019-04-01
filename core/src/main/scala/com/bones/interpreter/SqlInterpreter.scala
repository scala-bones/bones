package com.bones.interpreter

import com.bones.data.Value._
import shapeless.{HList, Nat}

object SqlInterpreter {

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]) : String = {
    group match {
      case KvpNil => ""
      case op: KvpGroupHead[a, al, h, hl, t, tl] => s"${kvpGroup(op.head)}, ${kvpGroup(op.tail)}"
      case op: KvpSingleValueHead[h, t, tl, a] => s"${op.fieldDefinition.key} ${valueDefinition(op.fieldDefinition.op)}"
      case op: OptionalKvpGroup[h,hl] => s"${kvpGroup(op.kvpGroup)}"
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): String =
    fgo match {
      case op: OptionalValueDefinition[a] => valueDefinition(op.valueDefinitionOp) + " ! is not null"
      case ob: BooleanData => "boolean"
      case rs: StringData => "text"
      case ri: LongData => "int8"
      case uu: UuidData => "varchar(36)"
      case dd: DateTimeData => "timestamp"
      case bd: BigDecimalData => "number"
      case ld: ListData[t] => "list"
      case ed: EitherData[a,b] => s"${valueDefinition(ed.definitionA)} can be null , ${valueDefinition(ed.definitionB)} can be null"
      case esd: EnumerationStringData[a] => "text"
      case esd: EnumStringData[a] => "text"
      case gr: KvpGroupData[h,hl] => kvpGroup(gr.kvpGroup)
      case x: XMapData[h,al,b] => s"createOperation table ${x.manifestOfA.runtimeClass.getSimpleName}\n ${kvpGroup(x.from)}"

    }

}
