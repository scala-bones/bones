package com.bones.interpreter

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.{Base64, UUID}

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import net.liftweb.json.JsonAST._

object EncodeToJValueInterpreter {
  type EncodeToJValue[A] = A => JValue

}

trait EncodeToJValueInterpreter extends KvpOutputInterpreter[JValue] {

  def dateFormatter: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter

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

  override def shortToOut(sd: ShortData): Short => JValue =
    input => JInt(BigInt(input))

  override def longToOut(op: LongData): Long => JValue =
    input => JInt(BigInt(input))

  override def uuidToOut(op: UuidData): UUID => JValue =
    input => JString(input.toString)

  override def dateTimeToOut(op: LocalDateTimeData): LocalDateTime => JValue =
    input => JString(dateFormatter.format(input))

  override def localDateToOut(op: LocalDateData): LocalDate => JValue =
    input => JString(localDateFormatter.format(input))

  override def bigDecimalToOut(op: BigDecimalData): BigDecimal => JValue =
    input => JString(input.toString())

  override def toOutList(list: List[JValue]): JValue = JArray(list)

  override def enumerationToOut[E <: Enumeration, V: Manifest](op: EnumerationData[E, V]):
    op.enumeration.Value => JValue =
    input => JString(input.toString)


}
