package com.bones.sprayjson

import com.bones.data.{KeyDefinition, KvpCollection}
import com.bones.interpreter.encoder.KvpInterchangeFormatEncoderInterpreter
import spray.json.{JsObject, JsString, JsValue}

/** Module responsible for converting values to Circe JSON without validation. The entry point for
  * this class is [KvpInterchangeFormatEncoderInterpreter.encoderFromSchema].
  */
trait SprayEncoderInterpreter[ALG[_]] extends KvpInterchangeFormatEncoderInterpreter[ALG, JsValue] {

  override def addStringField(element: JsValue, name: String, value: String): JsValue = {
    val fields = element.asJsObject.fields.updated(name, JsString(value))
    JsObject(fields)
  }

  override def empty: JsValue = JsObject()

  override def combine(a: JsValue, b: JsValue): JsValue = {
    JsObject(a.asJsObject.fields ++ b.asJsObject.fields)
  }

  override def toObj[A](kvDef: KeyDefinition[String, ALG, A], value: JsValue): JsValue =
    JsObject((kvDef.key, value))

}
