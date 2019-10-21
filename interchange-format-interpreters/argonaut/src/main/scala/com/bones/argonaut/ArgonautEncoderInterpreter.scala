package com.bones.argonaut

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.util.{Base64, UUID}

import argonaut._
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter

object ArgonautEncoderInterpreter {
  val isoInterpreter = new ArgonautEncoderInterpreter {
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override def localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
  }
}
/**
  * Module responsible for converting values to Argonaut JSON without validation.
  * The entry point for this class is [KvpInterchangeFormatEncoderInterpreter.fromSchema].
  */
trait ArgonautEncoderInterpreter extends KvpInterchangeFormatEncoderInterpreter[Json] {

  def localDateFormatter: DateTimeFormatter
  def localDateTimeFormatter: DateTimeFormatter

  override def none: Json = Json.jNull

  override def empty: Json = Json.obj()

  override def combine(prefix: Json, postfix: Json): Json = {
    val values1 = prefix.obj.toList.flatMap(_.toList)
    val values2 = postfix.obj.toList.flatMap(_.toList)
    Json.obj(values1 ::: values2: _*)
  }

  override def toObj[A](kvDef: KeyValueDefinition[A], value: Json): Json =
    Json.obj((kvDef.key, value))

  override def booleanToOut(op: BooleanData): Boolean => Json =
    input => Json.jBool(input)

  override def stringToOut(op: StringData): String => Json =
    input => Json.jString(input)

  override def intToOut(op: IntData): Int => Json = Json.jNumber

  override def longToOut(op: LongData): Long => Json =
    input => Json.jNumber(input)

  override def uuidToOut(op: UuidData): UUID => Json =
    input => Json.jString(input.toString)

  override def dateTimeToOut(op: LocalDateTimeData): LocalDateTime => Json =
    input => Json.jString(localDateTimeFormatter.format(input))

  override def localDateToOut(op: LocalDateData): LocalDate => Json =
    input => Json.jString(localDateFormatter.format(input))

  override def floatToOut(op: FloatData): Float => Json =
    input => Json.jNumber(input.toDouble)

  override def doubleToOut(op: DoubleData): Double => Json =
    input => Json.jNumber(input)

  override def bigDecimalToOut(op: BigDecimalData): BigDecimal => Json =
    input => Json.jNumber(input)

  override def byteArrayToOut(ba: ByteArrayData): Array[Byte] => Json =
    input => Json.jString(Base64.getEncoder.encodeToString(input))

  override def toOutList(list: List[Json]): Json =
    Json.array(list :_*)

  override def shortToOut(sd: ShortData): Short => Json =
    input => Json.jNumber(input.toInt)

  override def enumerationToOut[E <: Enumeration, V: Manifest](op: EnumerationData[E, V]):
    op.enumeration.Value => Json = input => Json.jString(input.toString)

  override def addStringField(element: Json, name: String, value: String): Json =
    Json.obj((name, Json.jString(value)) :: element.obj.toList.flatMap(_.toList) :_*)

}
