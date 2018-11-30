package com.bones.bson

import java.time.ZonedDateTime

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDecimal, BSONDocument, BSONElement, BSONLong, BSONNull, BSONString, BSONValue}



object EncodeToBson extends KvpOutputInterpreter[BSONValue] {

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
  override def booleanToOut[A](op: BooleanData): A => BSONValue =
    input => BSONBoolean(input.asInstanceOf[Boolean])

  @inline
  override def stringToOut[A](op: StringData): A => BSONValue =
    input => BSONString(input.asInstanceOf[String])

  @inline
  override def longToOut[A](op: LongData): A => BSONValue =
    input => BSONLong(input.asInstanceOf[Long])

  @inline
  override def uuidToOut[A](op: UuidData): A => BSONValue =
    input => BSONString(input.toString)

  @inline
  override def dateTimeToOut[A](op: DateTimeData): A => BSONValue =
    input => BSONDateTime(input.asInstanceOf[ZonedDateTime].toEpochSecond)

  @inline
  override def bigDecimalToOut[A](op: BigDecimalData): A => BSONValue =
    input => BSONDecimal.fromBigDecimal(input.asInstanceOf[BigDecimal]).getOrElse(BSONString(input.toString))

  @inline
  override def listDataToOut[A, T, L<:List[T]](op: ListData[T, L]): A => BSONValue = {
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