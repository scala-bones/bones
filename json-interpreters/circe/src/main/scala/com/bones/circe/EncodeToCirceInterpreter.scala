package com.bones.circe

import java.time.ZonedDateTime
import java.util.UUID

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import io.circe._

object EncodeToCirceInterpreter extends KvpOutputInterpreter[Json] {
  override def none: Json = Json.Null

  override def empty: Json = Json.obj()

  override def appendGroup(prefix: Json, postfix: Json): Json = {
    val v1 = prefix.asObject.toList.flatMap(_.toList)
    val v2 = postfix.asObject.toList.flatMap(_.toList)
    Json.obj( v1 ::: v2 :_*)
  }


  override def toObj[A](kvDef: KeyValueDefinition[A], value: Json): Json =
    Json.obj( (kvDef.key, value) )

  override def booleanToOut[A](op: BooleanData): Boolean => Json =
    input => Json.fromBoolean(input)

  override def stringToOut[A](op: StringData): String => Json =
    input => Json.fromString(input)

  override def longToOut[A](op: LongData): Long => Json =
    input => Json.fromLong(input)

  override def uuidToOut[A](op: UuidData): UUID => Json =
    input => Json.fromString(input.toString)

  override def dateTimeToOut[A](op: DateTimeData): ZonedDateTime => Json =
    input => Json.fromString(op.dateFormat.format(input))

  override def bigDecimalToOut[A](op: BigDecimalData): BigDecimal => Json =
    input => Json.fromBigDecimal(input)

  override def listDataToOut[A, T](op: ListData[T]): A => Json =
    input => Json.arr(input.asInstanceOf[List[Json]] :_*)

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => Json =
    input => Json.fromString(input.toString)

  override def enumToOut[A](op: EnumStringData[_]): A => Json =
    input => Json.fromString(input.toString)

}
