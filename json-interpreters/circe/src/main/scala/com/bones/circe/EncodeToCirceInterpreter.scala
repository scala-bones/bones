package com.bones.circe

import java.time.ZonedDateTime

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import io.circe.Json.JObject
import io.circe._
import shapeless.{HList, HNil, Nat}

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

  override def booleanToOut[A](op: BooleanData): A => Json =
    input => Json.fromBoolean(input.asInstanceOf[Boolean])

  override def stringToOut[A](op: StringData): A => Json =
    input => Json.fromString(input.toString)

  override def longToOut[A](op: LongData): A => Json =
    input => Json.fromLong(input.asInstanceOf[Long])

  override def uuidToOut[A](op: UuidData): A => Json =
    input => Json.fromString(input.toString)

  override def dateTimeToOut[A](op: DateTimeData): A => Json =
    input => Json.fromString(op.dateFormat.format(input.asInstanceOf[ZonedDateTime]))

  override def bigDecimalToOut[A](op: BigDecimalData): A => Json =
    input => Json.fromBigDecimal(input.asInstanceOf[BigDecimal])

  override def listDataToOut[A, T, L <: List[T]](op: ListData[T, L]): A => Json =
    input => Json.arr(input.asInstanceOf[List[Json]] :_*)

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => Json =
    input => Json.fromString(input.toString)

  override def enumToOut[A](op: EnumStringData[_]): A => Json =
    input => Json.fromString(input.toString)

}
