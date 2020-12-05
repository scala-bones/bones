package com.bones.json4s.impl

import java.util.Base64

import com.bones.interpreter.encoder.InterchangeFormatPrimitiveEncoder
import org.json4s.{JArray, JBool, JDecimal, JInt, JLong, JNull, JObject, JString, JValue}

object Json4sPrimitiveEncoder extends InterchangeFormatPrimitiveEncoder[JValue] {
  override def none: JValue = JNull

  override def empty: JValue = JObject()

  override def booleanToOut: Boolean => JValue =
    input => JBool(input)

  override def stringToOut: String => JValue =
    input => JString(input)

  override def intToOut: Int => JValue =
    JInt(_)

  override def floatToOut: Float => JValue =
    float => JDecimal(new java.math.BigDecimal(float))

  override def doubleToOut: Double => JValue =
    double => JDecimal(new java.math.BigDecimal(double))

  override def byteArrayToOut: Array[Byte] => JValue =
    input => JString(Base64.getEncoder.encodeToString(input))

  override def longToOut: Long => JValue =
    JLong(_)

  override def shortToOut: Short => JValue =
    JInt(_)

  override def bigDecimalToOut: BigDecimal => JValue =
    bd => JDecimal(bd.bigDecimal)

  override def toOutList(list: List[JValue]): JValue = JArray(list)

  override def addStringField(element: JValue, name: String, value: String): JValue = {
    element match {
      case obj: JObject => {
        val newFields = (name, JString(value)) :: obj.obj
        JObject(newFields: _*)
      }
      case _ => sys.error(s"Expected JObject in addStringField, received: ${element}")
    }
  }

  /** Assumes prefix and postfix are JValue objects and combines the key/value pairs into a single object. */
  override def combine(prefix: JValue, postfix: JValue): JValue = {
    val newFields = for {
      JObject(f1) <- prefix
      JObject(f2) <- postfix
    } yield f1 ::: f2
    JObject(newFields.flatten)
  }

}
