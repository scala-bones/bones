package com.bones.bson

import java.util.Base64

import com.bones.bson.BsonPrimitiveEncoder.combine
import com.bones.data.KeyDefinition
import com.bones.interpreter.encoder.KvpInterchangeFormatEncoderInterpreter
import reactivemongo.bson.buffer.ArrayBSONBuffer
import reactivemongo.bson.{
  BSONArray,
  BSONBoolean,
  BSONDecimal,
  BSONDocument,
  BSONElement,
  BSONInteger,
  BSONLong,
  BSONNull,
  BSONString,
  BSONValue
}

/** Responsible for creating functions to encode values to BSON data. See
  * [KvpInterchangeFormatEncoderInterpreter.encoderFromSchema] for the entry point into this module.
  */
trait BsonEncoderInterpreter[ALG[_]]
    extends KvpInterchangeFormatEncoderInterpreter[ALG, BSONValue] {

  override def combine(pre: BSONValue, post: BSONValue): BSONValue = {
    (pre, post) match {
      case (BSONDocument(preElement), BSONDocument(postElements)) =>
        BSONDocument(preElement ++ postElements)
      case _ =>
        throw new RuntimeException("pre and post must be BSONDocument options")
    }
  }

  override def empty: BSONValue = BSONDocument.empty

  override def addStringField(element: BSONValue, name: String, value: String): BSONValue =
    element match {
      case doc: BSONDocument => combine(doc, BSONDocument(BSONElement(name, BSONString(value))))
      case _                 => element
    }
  override val coproductTypeKey: String = "type"

  override def toObj[A](kvDef: KeyDefinition[String, ALG, A], value: BSONValue): BSONValue =
    BSONDocument(BSONElement(kvDef.key, value))

}
