package com.bones.bson

import java.util.Base64

import com.bones.interpreter.InterchangeFormatPrimitiveEncoder
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

object BsonPrimitiveEncoder extends InterchangeFormatPrimitiveEncoder[BSONValue] {

  override def booleanToOut: Boolean => BSONValue =
    input => BSONBoolean(input)

  override def stringToOut: String => BSONValue =
    input => BSONString(input)

  override def shortToOut: Short => BSONValue =
    input => BSONInteger(input.toInt)

  override def intToOut: Int => BSONValue =
    input => BSONInteger(input)

  override def longToOut: Long => BSONValue =
    input => BSONLong(input)

  override def floatToOut: Float => BSONValue =
    input => BSONDecimal.fromBigDecimal(BigDecimal(input)).getOrElse(BSONString(input.toString))

  override def doubleToOut: Double => BSONValue =
    input => BSONDecimal.fromBigDecimal(BigDecimal(input)).getOrElse(BSONString(input.toString))

  /** TODO:, need to propagate the error instead of hiding with the string conversion */
  override def bigDecimalToOut: BigDecimal => BSONValue =
    input => BSONDecimal.fromBigDecimal(input).getOrElse(BSONString(input.toString))

  override def toOutList(list: List[BSONValue]): BSONValue = BSONArray(list)

  override def byteArrayToOut: Array[Byte] => BSONValue =
    input => BSONString(Base64.getEncoder.encodeToString(input))

  override val none: BSONValue = BSONNull
  override val empty: BSONValue = BSONDocument.empty

  override def combine(pre: BSONValue, post: BSONValue): BSONValue = {
    (pre, post) match {
      case (BSONDocument(preElement), BSONDocument(postElements)) =>
        BSONDocument(preElement ++ postElements)
      case _ =>
        throw new RuntimeException("pre and post must be BSONDocument options")
    }
  }

  override def addStringField(element: BSONValue, name: String, value: String): BSONValue =
    element match {
      case doc: BSONDocument => combine(doc, BSONDocument(BSONElement(name, BSONString(value))))
      case _                 => element
    }

}
