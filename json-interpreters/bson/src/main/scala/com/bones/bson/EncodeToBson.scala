package com.bones.bson

import java.time.ZonedDateTime
import java.util.UUID

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import reactivemongo.bson.buffer.ArrayBSONBuffer
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDecimal, BSONDocument, BSONElement, BSONLong, BSONNull, BSONString, BSONValue}



object EncodeToBson extends KvpOutputInterpreter[BSONValue] {

  def bsonResultToBytes(bsonValue: BSONValue) : Array[Byte] = {
    val buffer = new ArrayBSONBuffer()
    BSONDocument.write(bsonValue.asInstanceOf[BSONDocument], buffer)
    buffer.array
  }


  @inline
  val none: BSONValue = BSONNull
  @inline
  val empty: BSONValue = BSONDocument.empty
  @inline
  def appendGroup(pre: BSONValue, post: BSONValue) : BSONValue = {
    (pre, post) match {
      case (BSONDocument(preElement), BSONDocument(postElements)) => BSONDocument(preElement.append(postElements))
      case _ => throw new RuntimeException("pre and post must be BSONDocument options")
    }
  }

  @inline
  override def booleanToOut[A](op: BooleanData): Boolean => BSONValue =
    input => BSONBoolean(input)

  @inline
  override def stringToOut[A](op: StringData): String => BSONValue =
    input => BSONString(input)

  @inline
  override def longToOut[A](op: LongData): Long => BSONValue =
    input => BSONLong(input)

  @inline
  override def uuidToOut[A](op: UuidData): UUID => BSONValue =
    input => BSONString(input.toString)

  @inline
  override def dateTimeToOut[A](op: DateTimeData): ZonedDateTime => BSONValue =
    input => BSONDateTime(input.toEpochSecond)

  @inline
  override def bigDecimalToOut[A](op: BigDecimalData): BigDecimal => BSONValue =
    input => BSONDecimal.fromBigDecimal(input).getOrElse(BSONString(input.toString))

  @inline
  override def listDataToOut[A, T](op: ListData[T]): A => BSONValue = {
    val f = valueDefinition(op.tDefinition)
    (input: A) => {
      BSONArray(input.asInstanceOf[List[T]].map(i => f(i)))
    }
  }


  override def enumerationToOut[A](op: EnumerationStringData[A]): A => BSONValue =
    input => BSONString(input.toString)

  override def enumToOut[A](op: EnumStringData[_]): A => BSONValue =
    input => BSONString(input.toString)

  def toObj[A](kvDef: KeyValueDefinition[A], value: BSONValue): BSONValue =
    BSONDocument(BSONElement(kvDef.key, value))

}
