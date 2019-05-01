package com.bones.interpreter

import java.time.ZonedDateTime
import java.util.UUID

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import net.liftweb.json.JsonAST._


object EncodeToJValueInterpreter extends KvpOutputInterpreter[JValue] {
  type EncodeToJValue[A] = A => JValue

  override def none: JValue = JNull

  override def empty: JValue = JObject()

  override def combine(prefix: JValue, postfix: JValue): JValue =
    (prefix, postfix) match {
      case ( JObject(fields), JObject(fields2) ) => JObject(fields ::: fields2)
      case (_,_) => JObject()
    }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: JValue): JValue =
    JObject(JField(kvDef.key, value))

  override def booleanToOut[A](op: BooleanData): Boolean => JValue =
    input => JBool(input)

  override def stringToOut[A](op: StringData): String => JValue =
    input => JString(input)

  override def longToOut[A](op: LongData): Long => JValue =
    input => JInt(BigInt(input))

  override def uuidToOut[A](op: UuidData): UUID => JValue =
    input => JString(input.toString)

  override def dateTimeToOut[A](op: DateTimeData): ZonedDateTime => JValue =
    input => JString(op.dateFormat.format(input))

  override def bigDecimalToOut[A](op: BigDecimalData): BigDecimal => JValue =
    input => JString(input.toString())

  override def listDataToOut[A, T](op: ListData[T]): A => JValue =
    input => JArray(input.asInstanceOf[List[JValue]])

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => JValue =
    input => JString(input.toString)

  override def enumToOut[A](op: EnumStringData[_]): A => JValue =
    input => JString(input.toString)

}

