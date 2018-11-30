package com.bones.interpreter

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
  def booleanToOut[A](op: BooleanData): A => OUT
  def stringToOut[A](op: StringData): A => OUT
  def longToOut[A](op: LongData): A => OUT
  def uuidToOut[A](op: UuidData): A => OUT
  def dateTimeToOut[A](op: DateTimeData): A => OUT
  def bigDecimalToOut[A](op: BigDecimalData): A => OUT
  def listDataToOut[A,T,L<: List[T]](op: ListData[T,L]): A => OUT
  def enumerationToOut[A](op: EnumerationStringData[A]): A => OUT
  def enumToOut[A](op: EnumStringData[_]): A => OUT


  def dataClass[A](dc: DataClass[A]): A => OUT = {
    dc match {
      case x: XMapData[a,al,b] =>
        val groupF = kvpGroup(x.from)
        input: A => groupF.apply(x.fba(input))
      case op: OptionalDataClass[a] =>
        val dataClassF = dataClass(op.value)
        a: A => a match {
          case Some(x) => dataClassF.apply(x)
          case None => none
        }
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): H => OUT =
    group match {
      case KvpNil => (input: H) => none
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
      case op: KvpDataClassHead[h,t,tl,o] => {
        val hF = dataClass(op.dataClass)
        val tailF = kvpGroup(op.tail)
        (input: H) => {
          val head = hF(input.head)
          val tail = tailF(input.tail)
          appendGroup(head, tail)
        }
      }
      case op: OptionalKvpGroup[h,hl] =>
        val oF = kvpGroup(op.kvpGroup)
        input: H => input.head match {
          case Some(kvp) => oF(kvp)
          case None => none
        }
    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): A => OUT =
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
      case ld: ListData[t,l] => listDataToOut(ld)
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
    }

}
