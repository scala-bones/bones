package com.bones.react

import com.bones.data.Value._
import shapeless.{HList, Nat}

/**
  * Responsible for converting Keys into Human Readable names.
  * This is currently not used.
  */
object FlattenedHeaderInterpreter {

  type Key = String

  def fromSchema(bonesSchema: BonesSchema[_]): List[String] = {
    bonesSchema match {
      case x: HListConvert[a,al,b] => valueDefinition(x)(None)
    }
  }

  def kvpHList[H<:HList,HL<:Nat](input: KvpHList[H,HL]): List[String] = {
    input match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        valueDefinition(op.fieldDefinition.op)(Some(op.fieldDefinition.key)) ++ kvpHList(op.tail)
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        kvpHList(op.head) ++ kvpHList(op.tail)
      case op: KvpConcreteTypeHead[a,ht,nt,ho,xl,xll] =>
        kvpHList(op.hListConvert.from) ++ kvpHList(op.tail)
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

  def valueDefinition[A](fgo: KvpValue[A]): Option[Key] => List[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        valueDefinition(op)
      case BooleanData(_) => keyToName
      case StringData(_) => keyToName
      case ShortData(_) => keyToName
      case IntData(_) => keyToName
      case LongData(_) => keyToName
      case UuidData(_) => keyToName
      case LocalDateData(_) => keyToName
      case LocalDateTimeData(_) => keyToName
      case FloatData(_) => keyToName
      case DoubleData(_) => keyToName
      case BigDecimalData(_) => keyToName
      case ListData(_,_) => keyToName
      case EitherData(_,_) => keyToName
      case ByteArrayData(_) => keyToName
      case EnumerationData(_,_) => keyToName
      case kvp: KvpHListValue[h,hl] =>
        _ => kvpHList(kvp.kvpHList)
      case x: HListConvert[a,al,b] =>
        _ => kvpHList(x.from)
    }

}
