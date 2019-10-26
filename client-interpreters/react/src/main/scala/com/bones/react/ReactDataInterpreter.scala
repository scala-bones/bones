package com.bones.react

import java.time.format.DateTimeFormatter

import com.bones.data.Value._
import shapeless.{HList, Nat}

/**
  * Responsible for generating a list of [ReactComponentData] which is used by this react component
  * generate to send, retrieve and display fields in a component.
  */
object ReactDataInterpreter {

  case class KeyHierarchy(key: String, children: List[KeyHierarchy])
  case class ReactComponentData(realTypes: String, defaultState: String, keyValues: List[KeyHierarchy])

  private val jsDateTimeFormat = DateTimeFormatter.ofPattern("YYYY-MM-DD'T'HH:mm:ss'Z'")

  type Key = String

  def fromSchema(schema: BonesSchema[_]): List[ReactComponentData] = {
    schema match {
      case x: HListConvert[a,al,b] => valueDefinition(x)(None)
    }
  }

  def kvpHList[H<:HList,HL<:Nat](group: KvpHList[H,HL]): List[ReactComponentData] = {
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        valueDefinition(op.fieldDefinition.op)(Some(op.fieldDefinition.key)) ++ kvpHList(op.tail)
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        kvpHList(op.head) ++ kvpHList(op.tail)
      case op: KvpConcreteTypeHead[a,ht,nt,ho,xl,xll] =>
        kvpHList(op.hListConvert.from) ++ kvpHList(op.tail)
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): Option[Key] => List[ReactComponentData] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        valueDefinition(op.valueDefinitionOp)
      case ob: BooleanData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'boolean'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case rs: StringData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'string'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case sd: ShortData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'integer'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case id: IntData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'integer'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case ri: LongData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'integer'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case uu: UuidData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'string'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case dd: LocalDateData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'date'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case dd: LocalDateTimeData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'date'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case dd: FloatData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'float'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case dd: DoubleData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'float'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case bd: BigDecimalData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'float'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case ba: ByteArrayData =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'file'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case esd: EnumerationData[e,a] =>
        keyOpt => keyOpt.map(key => ReactComponentData(s"${key}:'enumeration'", s"${key}:''", List(KeyHierarchy(key, List.empty)))).toList
      case kvp: KvpHListValue[h,hl] =>
        val groupData = kvpHList(kvp.kvpHList)
        keyOpt => {
          keyOpt.map(key => {
            val realTypes = s"""${key}:{ ${groupData.map(_.realTypes).mkString(",")}"""
            val defaultState = s"""${key}: ${groupData.map(_.defaultState).mkString(",")}"""
            ReactComponentData(realTypes, defaultState, List(KeyHierarchy(key, groupData.flatMap(_.keyValues))))
          }).toList
        }
      case x: HListConvert[a,al,b] =>
        val groupData = kvpHList(x.from)
        keyOpt => {
          keyOpt match {
            case Some(key) => {
              val realTypes = s"""${key}:{ ${groupData.map(_.realTypes).mkString(",")} } """
              val defaultState = s"""${key}: { ${groupData.map(_.defaultState).mkString(",")} }"""
              List(ReactComponentData(realTypes, defaultState, List(KeyHierarchy(key, groupData.flatMap(_.keyValues)))))
            }
            case None => groupData
          }
        }
    }
}
