package com.bones.interpreter

import java.time.ZonedDateTime
import java.util.{Base64, UUID}

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import net.liftweb.json.JsonAST._

object EncodeToJValueInterpreter extends KvpOutputInterpreter[JValue] {
  type EncodeToJValue[A] = A => JValue

  override def none: JValue = JNull

  override def empty: JValue = JObject()

  override def combine(prefix: JValue, postfix: JValue): JValue =
    (prefix, postfix) match {
      case (JObject(fields), JObject(fields2)) => JObject(fields ::: fields2)
      case (_, _)                              => JObject()
    }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: JValue): JValue =
    JObject(JField(kvDef.key, value))

  override def booleanToOut(op: BooleanData): Boolean => JValue =
    input => JBool(input)

  override def stringToOut(op: StringData): String => JValue =
    input => JString(input)

  override def intToOut(op: IntData): Int => JValue =
    input => JInt(BigInt(input))

  override def floatToOut(op: FloatData): Float => JValue =
    input => JDouble(input.toDouble)

  override def doubleToOut(op: DoubleData): Double => JValue =
    input => JDouble(input)

  override def byteArrayToOut(ba: ByteArrayData): Array[Byte] => JValue =
    input => JString(Base64.getEncoder.encodeToString(input))

  override def longToOut(op: LongData): Long => JValue =
    input => JInt(BigInt(input))

  override def uuidToOut(op: UuidData): UUID => JValue =
    input => JString(input.toString)

  override def dateTimeToOut(op: DateTimeData): ZonedDateTime => JValue =
    input => JString(op.dateFormat.format(input))

  override def bigDecimalToOut(op: BigDecimalData): BigDecimal => JValue =
    input => JString(input.toString())

  override def listDataToOut[A, T](op: ListData[T]): A => JValue =
    input => JArray(input.asInstanceOf[List[JValue]])

  override def enumerationToOut[A](op: EnumerationStringData[A]): A => JValue =
    input => JString(input.toString)

}
