package com.bones.json4s

import com.bones.data.KeyDefinition
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import org.json4s.{JObject, JString, JValue}

trait Json4sEncoderInterpreter[ALG[_]] extends KvpInterchangeFormatEncoderInterpreter[ALG, JValue] {

  override def addStringField(element: JValue, name: String, value: String): JValue = {

    element match {
      case obj: JObject => {
        val newFields = (name, JString(value)) :: obj.obj
        JObject(newFields: _*)
      }
      case _ => sys.error(s"Expected JObject in Json4sEncoderInterpreter, received: ${element}")
    }
  }

  override def empty: JValue = JObject()

  override def combine(a: JValue, b: JValue): JValue = {
    val newFields = for {
      JObject(f1) <- a
      JObject(f2) <- b
    } yield f1 ::: f2
    JObject(newFields.flatten)
  }

  override def toObj[A](kvDef: KeyDefinition[String, ALG, A], value: JValue): JValue =
    JObject((kvDef.key, value))
}
