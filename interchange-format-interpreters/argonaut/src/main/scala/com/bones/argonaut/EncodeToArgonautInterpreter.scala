package com.bones.argonaut

import java.time.ZonedDateTime
import java.util.UUID

import argonaut._
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter

object EncodeToArgonautInterpreter extends KvpOutputInterpreter[Json] {

  override def none: Json = Json.jNull

  override def empty: Json = Json.obj()

  override def combine(prefix: Json, postfix: Json): Json = {
    val values1 = prefix.obj.toList.flatMap(_.toList)
    val values2 = postfix.obj.toList.flatMap(_.toList)
    Json.obj(values1 ::: values2: _*)
  }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: Json): Json =
    Json.obj((kvDef.key, value))

  override def booleanToOut[A](op: BooleanData): Boolean => Json =
    input => Json.jBool(input)

  override def stringToOut[A](op: StringData): String => Json =
    input => Json.jString(input)

  override def longToOut[A](op: LongData): Long => Json =
    input => Json.jNumber(input)

  override def uuidToOut[A](op: UuidData): UUID => Json =
    input => Json.jString(input.toString)

  override def dateTimeToOut[A](op: DateTimeData): ZonedDateTime => Json =
    input => Json.jString(op.dateFormat.format(input))

  override def bigDecimalToOut[A](op: BigDecimalData): BigDecimal => Json =
    input => Json.jNumber(input)

  override def listDataToOut[A, T](op: ListData[T]): A => Json =
    input => Json.array(input.asInstanceOf[List[Json]]: _*)

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => Json =
    input => Json.jString(input.toString)

}
