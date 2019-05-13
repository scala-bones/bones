package com.bones.jdbc

import java.util.{Calendar, TimeZone}

import com.bones.data.Value._
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{HList, Nat}

object FindInterpreter {

  val utcCalendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
  type FieldName = String
  type Path = List[String]

}

object TableName {
  def getTableName[B](dc: BonesSchema[B]): String = dc match {
    case t: HListConvert[a, al, b] =>
      camelToSnake(t.manifestOfA.runtimeClass.getSimpleName)
  }
}

object FieldNames {

  def fromSchema[A](dc: BonesSchema[A]): List[String] =
    dc match {
      case t: HListConvert[a, al, b] => kvpHList(t.from)
    }

  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL]): List[String] =
    group match {
      case KvpNil => List.empty
      case op: KvpSingleValueHead[h, t, tl, a] =>
        List(camelToSnake(op.fieldDefinition.key)) ::: kvpHList(op.tail)
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] =>
        kvpHList(op.hListConvert.from) ::: kvpHList(op.tail)
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        kvpHList(op.head) ::: kvpHList(op.tail)
    }

  def valueDefinition[A](fgo: KvpValue[A]): List[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[a] =>
        valueDefinition(op.valueDefinitionOp)
      case ob: BooleanData               => List.empty
      case rs: StringData                => List.empty
      case id: IntData  => List.empty
      case ri: LongData                  => List.empty
      case uu: UuidData                  => List.empty
      case dd: DateTimeData              => List.empty
      case bd: BigDecimalData            => List.empty
      case fd: FloatData => List.empty
      case dd: DoubleData => List.empty
      case ba: ByteArrayData             => List.empty
      case ld: ListData[t]               => List.empty
      case ed: EitherData[a, b]          => List.empty
      case esd: EnumerationStringData[a] => List.empty
      case st: SumTypeData[a, b]         => valueDefinition(st.from)
      case kvp: KvpHListValue[h, hl]     => kvpHList(kvp.kvpHList)
      case x: HListConvert[_, _, _]      => kvpHList(x.from)
    }

}
