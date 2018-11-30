package com.bones.argonaut

import java.time.ZonedDateTime

import argonaut._
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter

object EncodeToArgonautInterpreter extends KvpOutputInterpreter[Json]{
  override def none: Json = Json.jNull

  override def empty: Json = Json.obj()

  override def appendGroup(prefix: Json, postfix: Json): Json = {
    val values1 = prefix.obj.toList.flatMap(_.toList)
    val values2 = postfix.obj.toList.flatMap(_.toList)
    Json.obj( values1 ::: values2 :_*)
  }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: Json): Json =
    Json.obj( (kvDef.key, value) )

  override def booleanToOut[A](op: BooleanData): A => Json =
    input => Json.jBool(input.asInstanceOf[Boolean])

  override def stringToOut[A](op: StringData): A => Json =
    input => Json.jString(input.toString)

  override def longToOut[A](op: LongData): A => Json =
    input => Json.jNumber(input.asInstanceOf[Long])

  override def uuidToOut[A](op: UuidData): A => Json =
    input => Json.jString(input.toString)

  override def dateTimeToOut[A](op: DateTimeData): A => Json =
    input => Json.jString(op.dateFormat.format(input.asInstanceOf[ZonedDateTime]))

  override def bigDecimalToOut[A](op: BigDecimalData): A => Json =
    input => Json.jNumber(input.asInstanceOf[BigDecimal])

  override def listDataToOut[A, T, L <: List[T]](op: ListData[T, L]): A => Json =
    input => Json.array(input.asInstanceOf[List[Json]] :_*)

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => Json =
    input => Json.jString(input.toString)

  override def enumToOut[A](op: EnumStringData[_]): A => Json =
    input => Json.jString(input.toString)
}
