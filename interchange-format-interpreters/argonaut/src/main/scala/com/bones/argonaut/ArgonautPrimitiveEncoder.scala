package com.bones.argonaut

import java.util.Base64

import argonaut.Json
import com.bones.interpreter.encoder.InterchangeFormatPrimitiveEncoder

object ArgonautPrimitiveEncoder extends InterchangeFormatPrimitiveEncoder[Json] {
  override def none: Json = Json.jNull

  override def empty: Json = Json.obj()

  override def combine(prefix: Json, postfix: Json): Json = {
    val values1 = prefix.obj.toList.flatMap(_.toList)
    val values2 = postfix.obj.toList.flatMap(_.toList)
    Json.obj(values1 ::: values2: _*)
  }

  /** Create a function which converts a boolean into the specific OUT type */
  override def booleanToOut: Boolean => Json =
    input => Json.jBool(input)

  /** Create a function which converts a String into the specific OUT type */
  override def stringToOut: String => Json =
    input => Json.jString(input)

  override def intToOut: Int => Json = Json.jNumber

  override def longToOut: Long => Json =
    input => Json.jNumber(input)

  override def floatToOut: Float => Json =
    input => Json.jNumber(input.toDouble)

  override def doubleToOut: Double => Json =
    input => Json.jNumber(input)

  override def bigDecimalToOut: BigDecimal => Json =
    input => Json.jNumber(input)

  override def byteArrayToOut: Array[Byte] => Json =
    input => Json.jString(Base64.getEncoder.encodeToString(input))

  override def toOutList(list: List[Json]): Json =
    Json.array(list: _*)

  override def shortToOut: Short => Json =
    input => Json.jNumber(input.toInt)

  override def addStringField(element: Json, name: String, value: String): Json =
    Json.obj((name, Json.jString(value)) :: element.obj.toList.flatMap(_.toList): _*)

}
