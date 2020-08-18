package com.bones.circe

import com.bones.data.KeyDefinition
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import io.circe._

/**
  * Module responsible for converting values to Circe JSON without validation.
  * The entry point for this class is [KvpInterchangeFormatEncoderInterpreter.encoderFromSchema].
  */
trait CirceEncoderInterpreter[ALG[_]] extends KvpInterchangeFormatEncoderInterpreter[ALG, Json] {

  override def addStringField(element: Json, name: String, value: String): Json = {
    val fields = (name, Json.fromString(value)) :: element.asObject.toList.flatMap(_.toList)
    Json.obj(fields: _*)
  }

  override def empty: Json = Json.obj()

  override def combine(a: Json, b: Json): Json = {
    val fields = a.asObject.toList.flatMap(_.toList) ::: b.asObject.toList.flatMap(_.toList)
    Json.obj(fields: _*)
  }

  override def toObj[A](kvDef: KeyDefinition[ALG, A], value: Json): Json =
    Json.obj((kvDef.key, value))

}
