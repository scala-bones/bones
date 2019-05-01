package com.bones.react

import com.bones.data.Value._
import shapeless.{HList, Nat}

object FlattenedHeaderInterpreter {

  type Key = String

  def fromSchema(bonesSchema: BonesSchema[_]): List[String] = {
    bonesSchema match {
      case x: XMapData[a,al,b] => valueDefinition(x)(None)
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): List[String] = {
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        valueDefinition(op.fieldDefinition.op)(Some(op.fieldDefinition.key)) ++ kvpGroup(op.tail)
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        kvpGroup(op.head) ++ kvpGroup(op.tail)
      case op: OptionalKvpGroup[h,hl] => ???
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] =>
        kvpGroup(op.xmapData.from) ++ kvpGroup(op.tail)
    }
  }

  // https://stackoverflow.com/questions/2559759/how-do-i-convert-camelcase-into-human-readable-names-in-java
  private def keyToName(keyOpt: Option[String]): List[String] =
    keyOpt.map(key => {
      key.replaceAll(
        String.format("%s|%s|%s",
          "(?<=[A-Z])(?=[A-Z][a-z])",
          "(?<=[^A-Z])(?=[A-Z])",
          "(?<=[A-Za-z])(?=[^A-Za-z])"
        ),
        " "
      )}).toList

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Option[Key] => List[String] =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        valueDefinition(op)
      case ob: BooleanData => keyToName
      case rs: StringData => keyToName
      case ri: LongData => keyToName
      case uu: UuidData => keyToName
      case dd: DateTimeData => keyToName
      case bd: BigDecimalData => keyToName
      case ld: ListData[t] => keyToName
      case ed: EitherData[a,b] => keyToName
      case ba: ByteArrayData => keyToName
      case esd: EnumerationStringData[a] => keyToName
      case esd: EnumStringData[a] => keyToName
      case kvp: KvpGroupData[h,hl] => {
        _ => kvpGroup(kvp.kvpGroup)
      }
      case x: XMapData[a,al,b] =>
        _ => kvpGroup(x.from)
      case s: SumTypeData[a,b] =>
        _ => valueDefinition(s.from)(None)
    }

}
