package com.bones.circe

import java.util.Base64

import com.bones.data.KeyValueDefinition
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import io.circe._

/**
  * Module responsible for converting values to Circe JSON without validation.
  * The entry point for this class is [KvpInterchangeFormatEncoderInterpreter.encoderFromSchema].
  */
trait CirceEncoderInterpreter extends KvpInterchangeFormatEncoderInterpreter[Json] {

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

  override def booleanToOut: Boolean => Json =
    input => Json.fromBoolean(input)

  override def stringToOut: String => Json =
    input => Json.fromString(input)

  override def intToOut: Int => Json =
    Json.fromInt

  override def floatToOut: Float => Json =
    i => Json.fromFloatOrNull(i)

  override def doubleToOut: Double => Json =
    d => Json.fromDoubleOrNull(d)

  override def byteArrayToOut: Array[Byte] => Json =
    input => Json.fromString(Base64.getEncoder.encodeToString(input))

  override def longToOut: Long => Json =
    input => Json.fromLong(input)

  override def shortToOut: Short => Json =
    input => Json.fromInt(input.toInt)

  override def bigDecimalToOut: BigDecimal => Json =
    input => Json.fromBigDecimal(input)

  override def toOutList(list: List[Json]): Json = Json.fromValues(list)

  override def addStringField(element: Json, name: String, value: String): Json =
    Json.obj((name, Json.fromString(value)) :: element.asObject.toList.flatMap(_.toList): _*)
}
