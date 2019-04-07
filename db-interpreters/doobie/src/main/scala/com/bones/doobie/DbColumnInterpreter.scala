package com.bones.doobie

import com.bones.data.Value._
import shapeless._, ops.hlist.IsHCons

object DbColumnInterpreter {

  type ColumnName = String
  type GroupToColumns[A] = A => List[String]
  type ToColumns[A] = ColumnName => A => List[String]


  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): GroupToColumns[H] = {
    group match {
      case KvpNil => _ => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] => {
        val headF = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailF = kvpGroup(op.tail)
        (input: H) => {
          val headResult = headF(input.head)
          val tailResult = tailF(input.tail)
          headResult ::: tailResult
        }
      }
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        val headF = kvpGroup(op.head)
        val tailF = kvpGroup(op.tail)
        (in: H) => {
          val cast = in.asInstanceOf[h :: t]
          headF(cast.head) ::: tailF(cast.tail)
        }
      case op: OptionalKvpGroup[h,hl] => ???
    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): ToColumns[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => {
        val valueF = valueDefinition(op.valueDefinitionOp)
        (name: ColumnName) => {
          val trueName = valueF(name)
          (a: Option[b]) => {
            a match {
              case None => List.empty
              case Some(input) => trueName(input)
            }
          }
        }
      }
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
      case x: XMapData[a,al,b] => ???
    }
}
