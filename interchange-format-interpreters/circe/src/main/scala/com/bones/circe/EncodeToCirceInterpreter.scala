package com.bones.circe

import java.time.ZonedDateTime
import java.util.{Base64, UUID}

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import io.circe._

object EncodeToCirceInterpreter extends KvpOutputInterpreter[Json] {
  override def none: Json = Json.Null

  override def empty: Json = Json.obj()

  override def combine(prefix: Json, postfix: Json): Json = {
    val v1 = prefix.asObject.toList.flatMap(_.toList)
    val v2 = postfix.asObject.toList.flatMap(_.toList)
    Json.obj(v1 ::: v2: _*)
  }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: Json): Json =
    Json.obj((kvDef.key, value))

  override def booleanToOut(op: BooleanData): Boolean => Json =
    input => Json.fromBoolean(input)

  override def stringToOut(op: StringData): String => Json =
    input => Json.fromString(input)

  override def intToOut(op: IntData): Int => Json =
    Json.fromInt

  override def floatToOut(op: FloatData): Float => Json =
    i => Json.fromFloatOrNull(i)

  override def doubleToOut(op: DoubleData): Double => Json =
    d => Json.fromDoubleOrNull(d)

  override def byteArrayToOut(ba: ByteArrayData): Array[Byte] => Json =
    input => Json.fromString(Base64.getEncoder.encodeToString(input))

  override def longToOut(op: LongData): Long => Json =
    input => Json.fromLong(input)

  override def uuidToOut(op: UuidData): UUID => Json =
    input => Json.fromString(input.toString)

  override def dateTimeToOut(op: DateTimeData): ZonedDateTime => Json =
    input => Json.fromString(op.dateFormat.format(input))

  override def bigDecimalToOut(op: BigDecimalData): BigDecimal => Json =
    input => Json.fromBigDecimal(input)

  override def listDataToOut[A, T](op: ListData[T]): A => Json =
    input => Json.arr(input.asInstanceOf[List[Json]]: _*)

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => Json =
    input => Json.fromString(input.toString)

}
