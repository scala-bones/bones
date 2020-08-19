package com.bones.argonaut

import java.util.Base64

import argonaut._
import com.bones.data.KeyDefinition
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter

/**
  * Module responsible for converting values to Argonaut JSON without validation.
  * The entry point for this class is [KvpInterchangeFormatEncoderInterpreter.encoderFromSchema].
  */
trait ArgonautEncoderInterpreter[ALG[_]] extends KvpInterchangeFormatEncoderInterpreter[ALG, Json] {

  override def combine(a: Json, b: Json): Json = {
    val values1 = a.obj.toList.flatMap(_.toList)
    val values2 = b.obj.toList.flatMap(_.toList)
    Json.obj(values1 ::: values2: _*)
  }

  override def empty: Json = Json.jEmptyObject

  override def addStringField(element: Json, name: String, value: String): Json =
    Json.obj((name, Json.jString(value)) :: element.obj.toList.flatMap(_.toList): _*)

  override def toObj[A](kvDef: KeyDefinition[ALG, A], value: Json): Json =
    Json.obj((kvDef.key, value))

}
