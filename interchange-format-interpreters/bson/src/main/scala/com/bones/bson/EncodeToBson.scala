package com.bones.bson

import java.time.ZonedDateTime
import java.util.{Base64, UUID}

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import reactivemongo.bson.buffer.ArrayBSONBuffer
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDecimal, BSONDocument, BSONElement, BSONInteger, BSONLong, BSONNull, BSONString, BSONValue}

object EncodeToBson extends KvpOutputInterpreter[BSONValue] {

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

  override def booleanToOut(op: BooleanData): Boolean => BSONValue =
    input => BSONBoolean(input)

  override def stringToOut(op: StringData): String => BSONValue =
    input => BSONString(input)

  override def intToOut(op: IntData): Int => BSONValue =
    input => BSONInteger(input)

  override def longToOut(op: LongData): Long => BSONValue =
    input => BSONLong(input)

  override def uuidToOut(op: UuidData): UUID => BSONValue =
    input => BSONString(input.toString)

  override def dateTimeToOut(op: DateTimeData): ZonedDateTime => BSONValue =
    input => BSONDateTime(input.toEpochSecond)


  override def floatToOut(op: FloatData): Float => BSONValue =
    input => BSONDecimal.fromBigDecimal(BigDecimal(input)).getOrElse(BSONString(input.toString))

  override def doubleToOut(op: DoubleData): Double => BSONValue =
    input => BSONDecimal.fromBigDecimal(BigDecimal(input)).getOrElse(BSONString(input.toString))

  /** TODO:, need to propagate the error instead of hiding with the string conversion */
  override def bigDecimalToOut(op: BigDecimalData): BigDecimal => BSONValue =
    input =>
      BSONDecimal.fromBigDecimal(input).getOrElse(BSONString(input.toString))

  override def toOutList(list: List[BSONValue]): BSONValue = BSONArray(list)

  override def byteArrayToOut(ba: ByteArrayData): Array[Byte] => BSONValue =
    input =>
      BSONString(Base64.getEncoder.encodeToString(input))

  override def enumerationToOut[A](
      op: EnumerationStringData[A]): A => BSONValue =
    input => BSONString(input.toString)

  def toObj[A](kvDef: KeyValueDefinition[A], value: BSONValue): BSONValue =
    BSONDocument(BSONElement(kvDef.key, value))

}
