package com.bones.interpreter

import java.time.ZonedDateTime

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import net.liftweb.json.JsonAST._
import shapeless.{HList, Nat}


object EncodeToJValueInterpreter extends KvpOutputInterpreter[JValue] {
  type EncodeToJValue[A] = A => JValue

  override def none: JValue = JNull

  override def empty: JValue = JObject()

  override def appendGroup(prefix: JValue, postfix: JValue): JValue =
    (prefix, postfix) match {
      case ( JObject(fields), JObject(fields2) ) => JObject(fields ::: fields2)
    }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: JValue): JValue =
    JObject(JField(kvDef.key, value))

  override def booleanToOut[A](op: BooleanData): A => JValue =
    input => JBool(input.asInstanceOf[Boolean])

  override def stringToOut[A](op: StringData): A => JValue =
    input => JString(input.toString)

  override def longToOut[A](op: LongData): A => JValue =
    input => JInt(BigInt(input.asInstanceOf[Long]))

  override def uuidToOut[A](op: UuidData): A => JValue =
    input => JString(input.toString)

  override def dateTimeToOut[A](op: DateTimeData): A => JValue =
    input => JString(op.dateFormat.format(input.asInstanceOf[ZonedDateTime]))

  override def bigDecimalToOut[A](op: BigDecimalData): A => JValue =
    input => JString(input.toString())

  override def listDataToOut[A, T, L <: List[T]](op: ListData[T, L]): A => JValue =
    input => JArray(input.asInstanceOf[List[JValue]])

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => JValue =
    input => JString(input.toString())

  override def enumToOut[A](op: EnumStringData[_]): A => JValue =
    input => JString(input.toString())

}

