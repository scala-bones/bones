package com.bones.circe

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import java.util.{Base64, UUID}

import com.bones.data.{KeyValueDefinition, _}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.NoAlgebraEncoder
import io.circe._

object CirceEncoderInterpreter {

  /**
    * Implementation of the [CirceEncoderInterpreter] specifying ISO String formatter for date and datetime.
    */
  val isoInterpreter: CirceEncoderInterpreter = new CirceEncoderInterpreter {
      override def localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
      override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    }

  val noAlgebraEncoder: NoAlgebraEncoder[Json] = NoAlgebraEncoder[Json]()
}

/**
  * Module responsible for converting values to Circe JSON without validation.
  * The entry point for this class is [KvpInterchangeFormatEncoderInterpreter.fromSchema].
  */
trait CirceEncoderInterpreter extends KvpInterchangeFormatEncoderInterpreter[Json] {

  def localDateTimeFormatter: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter

  override def none: Json = Json.Null

  override def empty: Json = Json.obj()

  /** Assumes prefix and postfix are JSON objects and combines the key/value pairs into a single object. */
  override def combine(prefix: Json, postfix: Json): Json = {
    val v1 = prefix.asObject.toList.flatMap(_.toList)
    val v2 = postfix.asObject.toList.flatMap(_.toList)
    Json.obj(v1 ::: v2: _*)
  }

  override def toObj[ALG[_], A](kvDef: KeyValueDefinition[ALG, A], value: Json): Json =
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

  override def shortToOut(sd: ShortData): Short => Json =
    input => Json.fromInt(input.toInt)

  override def uuidToOut(op: UuidData): UUID => Json =
    input => Json.fromString(input.toString)

  override def dateTimeToOut(op: LocalDateTimeData): LocalDateTime => Json =
    input => Json.fromString(localDateTimeFormatter.format(input))

  override def localDateToOut(op: LocalDateData): LocalDate => Json =
    input => Json.fromString(localDateFormatter.format(input))

  override def bigDecimalToOut(op: BigDecimalData): BigDecimal => Json =
    input => Json.fromBigDecimal(input)

  override def toOutList(list: List[Json]): Json = Json.fromValues(list)

  override def enumerationToOut[E <: Enumeration, V: Manifest](op: EnumerationData[E, V]):
    op.enumeration.Value => Json =
    input => Json.fromString(input.toString)

  override def addStringField(element: Json, name: String, value: String): Json =
    Json.obj((name, Json.fromString(value)) :: element.asObject.toList.flatMap(_.toList) :_*)
}
