package com.bones.sprayjson.impl

import java.util.Base64

import com.bones.interpreter.InterchangeFormatPrimitiveEncoder
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}

object SprayPrimitiveEncoder extends InterchangeFormatPrimitiveEncoder[JsValue] {
  override def none: JsValue = JsNull

  override def empty: JsValue = JsObject()

  override def booleanToOut: Boolean => JsValue =
    input => JsBoolean(input)

  override def stringToOut: String => JsValue =
    input => JsString(input)

  override def intToOut: Int => JsValue =
    JsNumber(_)

  override def floatToOut: Float => JsValue =
    JsNumber(_)

  override def doubleToOut: Double => JsValue =
    JsNumber(_)

  override def byteArrayToOut: Array[Byte] => JsValue =
    input => JsString(Base64.getEncoder.encodeToString(input))

  override def longToOut: Long => JsValue =
    JsNumber(_)

  override def shortToOut: Short => JsValue =
    JsNumber(_)

  override def bigDecimalToOut: BigDecimal => JsValue =
    JsNumber(_)

  override def toOutList(list: List[JsValue]): JsValue = JsArray(list.toVector)

  override def addStringField(element: JsValue, name: String, value: String): JsValue = {
    JsObject(element.asJsObject.fields.updated(name, JsString(value)))
  }

  /** Assumes prefix and postfix are JsValue objects and combines the key/value pairs into a single object. */
  override def combine(prefix: JsValue, postfix: JsValue): JsValue = {
    val v1 = prefix.asJsObject.fields
    val v2 = postfix.asJsObject.fields
    JsObject(v1 ++ v2)
  }

}
