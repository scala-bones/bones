package com.bones.interpreter

import java.time.ZonedDateTime
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import shapeless.{HList, Nat}

object KvpOutInterpreter {
  type FOUT[OUT,A] = A => Either[NonEmptyList[ExtractionError],A]
}
/**
  * Base trait for converting from HList or Case class to an interchange format such as JSON.
  * @tparam OUT The interchange format.
  */
trait KvpOutputInterpreter[OUT] {

  def none: OUT
  def empty: OUT
  def appendGroup(prefix: OUT, postfix: OUT): OUT
  def toObj[A](kvDef: KeyValueDefinition[A], value: OUT): OUT
  def booleanToOut[A](op: BooleanData): Boolean => OUT
  def stringToOut[A](op: StringData): String => OUT
  def longToOut[A](op: LongData): Long => OUT
  def uuidToOut[A](op: UuidData): UUID => OUT
  def dateTimeToOut[A](op: DateTimeData): ZonedDateTime => OUT
  def bigDecimalToOut[A](op: BigDecimalData): BigDecimal => OUT
  def listDataToOut[A,T](op: ListData[T]): A => OUT
  def enumerationToOut[A](op: EnumerationStringData[A]): A => OUT
  def enumToOut[A](op: EnumStringData[_]): A => OUT

  def fromSchema[A](bonesSchema: BonesSchema[A]): A => OUT = bonesSchema match {
    case x: XMapData[_,_,A] => valueDefinition(x)
  }


  protected def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): H => OUT =
    group match {
      case KvpNil => (input: H) => empty
      case op: KvpGroupHead[out,l,h,hl,t,tl] =>
        val headF = kvpGroup(op.head)
        val tailF = kvpGroup[t,tl](op.tail)
        (input: H) => {
          val l = op.split(input)
          val headOut = headF.apply(l._1)
          val tailOut = tailF.apply(l._2)
          appendGroup(headOut, tailOut)
        }
      case op: KvpSingleValueHead[h,t,tl,H] =>
        val valueF = valueDefinition(op.fieldDefinition.op)
        (input: H) => {
          import shapeless.::
          val cast = input.asInstanceOf[h :: t]
          val val1 = valueF.apply(cast.head)
          val tail = kvpGroup(op.tail)(cast.tail)
          appendGroup(toObj(op.fieldDefinition, val1), tail)
        }
      case op: OptionalKvpGroup[h,hl] =>
        val oF = kvpGroup(op.kvpGroup)
        input: H => input.head match {
          case Some(kvp) => oF(kvp)
          case None => none
        }
    }

  protected def valueDefinition[A](fgo: ValueDefinitionOp[A]): A => OUT =
    fgo match {
      case op: OptionalValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        (input: A) => {
          input match {
            case Some(x) => valueF(x)
            case None => none
          }
        }
      case ob: BooleanData => booleanToOut(ob)
      case rs: StringData => stringToOut(rs)
      case ri: LongData => longToOut(ri)
      case uu: UuidData => uuidToOut(uu)
      case dd: DateTimeData => dateTimeToOut(dd)
      case bd: BigDecimalData => bigDecimalToOut(bd)
      case ld: ListData[t] => listDataToOut(ld)
//      case ListData(vDefinition, _) => listDataToOut(vDefinition)
      case EitherData(aDefinition, bDefinition) =>
        val aF = valueDefinition(aDefinition)
        val bF = valueDefinition(bDefinition)
        (input: A) => {
          input match {
            case Left(aInput) => aF(aInput)
            case Right(bInput) => bF(bInput)
          }
        }
      case e: EnumerationStringData[a] => enumerationToOut(e)
      case e: EnumStringData[a] => enumToOut(e)
      case gd: KvpGroupData[h,hl] => {
        val fh = kvpGroup(gd.kvpGroup)
        input: A => fh(input.asInstanceOf[h])
      }
      case x: XMapData[h,hl,A] => {
        val fh = kvpGroup(x.from)
        input: A => {
          fh(x.fba(input))
        }
      }
    }

}
