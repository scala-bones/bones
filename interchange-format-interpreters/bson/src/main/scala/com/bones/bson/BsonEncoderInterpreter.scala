package com.bones.bson

import java.time.{LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.Base64

import com.bones.data.KeyValueDefinition
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import reactivemongo.bson.buffer.ArrayBSONBuffer
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDecimal, BSONDocument, BSONElement, BSONInteger, BSONLong, BSONNull, BSONString, BSONValue}

/**
  * Responsible for creating functions to encode values to BSON data.
  * See [KvpInterchangeFormatEncoderInterpreter.encoderFromSchema] for the entry point into this
  * module.
  */
object BsonEncoderInterpreter extends KvpInterchangeFormatEncoderInterpreter[BSONValue] {

  override val coproductTypeKey: String = "type"

  trait BsonEncoder[ALG[_]] extends InterchangeFormatEncoder[ALG, BSONValue]

  def bsonResultToBytes(bsonValue: BSONValue): Array[Byte] = {
    val buffer = new ArrayBSONBuffer()
    BSONDocument.write(bsonValue.asInstanceOf[BSONDocument], buffer)
    buffer.array
  }

  val none: BSONValue = BSONNull
  val empty: BSONValue = BSONDocument.empty

  def combine(pre: BSONValue, post: BSONValue): BSONValue = {
    (pre, post) match {
      case (BSONDocument(preElement), BSONDocument(postElements)) =>
        BSONDocument(preElement.append(postElements))
      case _ =>
        throw new RuntimeException("pre and post must be BSONDocument options")
    }
  }

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

  def toObj[ALG[_], A](kvDef: KeyValueDefinition[ALG, A], value: BSONValue): BSONValue =
    BSONDocument(BSONElement(kvDef.key, value))

  override def addStringField(element: BSONValue, name: String, value: String): BSONValue =
    element match {
      case doc: BSONDocument => combine(doc, BSONDocument(BSONElement(name, BSONString(value))))
      case _                 => element
    }
}
