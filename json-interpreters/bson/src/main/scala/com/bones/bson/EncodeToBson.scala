package com.bones.bson

import java.time.ZonedDateTime

import com.bones.data.Value._
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDecimal, BSONDocument, BSONElement, BSONLong, BSONNull, BSONString, BSONValue}
import shapeless.{HList, Nat}


object EncodeToBson {



  def dataClass[A](dc: DataClass[A]): A => BSONValue = {
    dc match {
      case x: XMapData[a,al,b] =>
        val groupF = kvpGroup(x.from)
        input: A => groupF.apply(x.fba(input))
      case op: OptionalDataClass[a] =>
        val dataClassF = dataClass(op.value)
        a: A => a match {
          case Some(x) => dataClassF.apply(x)
          case None => BSONNull
        }
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): H => BSONValue =
    group match {
      case KvpNil => (input: H) => BSONDocument.empty
      case op: KvpGroupHead[out,l,h,hl,t,tl] =>
        val headF = kvpGroup(op.head)
        val tailF = kvpGroup[t,tl](op.tail)
        (input: H) => {
          val l = op.split(input)
          val elements1 = headF.apply(l._1).asInstanceOf[BSONDocument].elements
          val elements2 = tailF.apply(l._2).asInstanceOf[BSONDocument].elements
          BSONDocument(elements1 append elements2)
        }
      case op: KvpSingleValueHead[h,t,tl,H] =>
        val valueF = valueDefinition(op.fieldDefinition.op)
        (input: H) => {
          import shapeless.::
          val cast = input.asInstanceOf[h :: t]
          val val1 = valueF.apply(cast.head)
          val elements = kvpGroup(op.tail)(cast.tail).asInstanceOf[BSONDocument].elements
          BSONDocument( BSONElement(op.fieldDefinition.key, val1) +: elements)
        }
      case op: KvpDataClassHead[h,t,tl,o] => {
        val hF = dataClass(op.dataClass)
        val tailF = kvpGroup(op.tail)
        (input: H) => {
          val hFields = hF(input.head).asInstanceOf[BSONDocument].elements
          val tailFields = tailF(input.tail).asInstanceOf[BSONDocument].elements
          BSONDocument(hFields append tailFields)
        }
      }
      case op: OptionalKvpGroup[h,hl] =>
        val oF = kvpGroup(op.kvpGroup)
        input: H => input.head match {
          case Some(kvp) => oF(kvp)
          case None => BSONNull
        }
    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): A => BSONValue =
    fgo match {
      case op: OptionalValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        (input: A) => {
          input match {
            case Some(x) => valueF(x)
            case None => BSONNull
          }
        }
      case ob: BooleanData => (input: A) => BSONBoolean(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => BSONString(input.asInstanceOf[String])
      case ri: LongData => (input: A) => BSONLong(input.asInstanceOf[Long].toInt)
      case uu: UuidData => (input: A) => BSONString(input.toString)
      case DateTimeData(format, _, validations) => (input: A) => BSONDateTime(input.asInstanceOf[ZonedDateTime].toEpochSecond)
      case bd: BigDecimalData => (input: A) => BSONDecimal.fromBigDecimal(input.asInstanceOf[BigDecimal]).getOrElse(BSONNull)
      case ld: ListData[t,l] =>
        val f = valueDefinition(ld.tDefinition)
        (input: A) => {
          BSONArray(input.asInstanceOf[List[t]].map(i => f(i)))
        }
      case EitherData(aDefinition, bDefinition) =>
        val aF = valueDefinition(aDefinition)
        val bF = valueDefinition(bDefinition)
        (input: A) => {
          input match {
            case Left(aInput) => aF(aInput)
            case Right(bInput) => bF(bInput)
          }
        }
      case EnumerationStringData(enumeration, validations) => (input: A) => BSONString(input.toString)
      case EnumStringData(enum, validations) => (input: A) => BSONString(input.toString)
      case gd: KvpGroupData[h,hl] => {
        val fh = kvpGroup(gd.kvpGroup)
        input: A => fh(input.asInstanceOf[h])
      }
    }
}
