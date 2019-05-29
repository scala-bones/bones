package com.bones.interpreter

import java.time.ZonedDateTime
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import shapeless.{HList, Nat, ::}

object KvpOutInterpreter {
  type FOUT[OUT, A] = A => Either[NonEmptyList[ExtractionError], A]
}

/**
  * Base trait for converting from HList or Case class to an interchange format such as JSON.
  * @tparam OUT The interchange format.
  */
trait KvpOutputInterpreter[OUT] {

  def none: OUT
  def empty: OUT

  /** Combine two groups of values */
  def combine(prefix: OUT, postfix: OUT): OUT
  def toObj[A](kvDef: KeyValueDefinition[A], value: OUT): OUT
  def booleanToOut(op: BooleanData): Boolean => OUT
  def stringToOut(op: StringData): String => OUT
  def intToOut(op: IntData): Int => OUT
  def longToOut(op: LongData): Long => OUT
  def uuidToOut(op: UuidData): UUID => OUT
  def dateTimeToOut(op: DateTimeData): ZonedDateTime => OUT
  def floatToOut(op: FloatData): Float => OUT
  def doubleToOut(op: DoubleData): Double => OUT
  def bigDecimalToOut(op: BigDecimalData): BigDecimal => OUT
  def byteArrayToOut(ba: ByteArrayData): Array[Byte] => OUT
  def toOutList(list: List[OUT]): OUT
  def enumerationToOut[A](op: EnumerationStringData[A]): A => OUT

  def fromSchema[A](bonesSchema: BonesSchema[A]): A => OUT = bonesSchema match {
    case x: HListConvert[_, _, A] => valueDefinition(x)
  }

  /** Interpreter for the KvpHList type. */
  protected def kvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): H => OUT =
    group match {
      case KvpNil =>
        (input: H) =>
          empty
      case op: KvpHListHead[out, l, h, hl, t, tl] =>
        val headF = kvpHList(op.head)
        val tailF = kvpHList[t, tl](op.tail)
        (input: H) =>
          {
            val l = op.split(input)
            val headOut = headF.apply(l._1)
            val tailOut = tailF.apply(l._2)
            combine(headOut, tailOut)
          }
      case op: KvpSingleValueHead[h, t, tl, H] =>
        val valueF = valueDefinition(op.fieldDefinition.op)
        val tailF = kvpHList(op.tail)
        implicit val hCons = op.isHCons
        (input: H) =>
          {
            val val1 = valueF(input.head)
            val tail = tailF(input.tail)
            combine(toObj(op.fieldDefinition, val1), tail)
          }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] => {
        val headF = kvpHList(op.hListConvert.from)
        val tailF = kvpHList(op.tail)
        implicit val hCons = op.isHCons
        (input: a :: ht) =>
          {
            val head = headF(op.hListConvert.fAtoH(input.head))
            val tail = tailF(input.tail)
            combine(head, tail)
          }
      }
    }

  protected def valueDefinition[A](fgo: KvpValue[A]): A => OUT =
    fgo match {
      case op: OptionalKvpValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        (input: A) =>
          {
            input match {
              case Some(x) => valueF(x)
              case None    => none
            }
          }
      case ob: BooleanData    => booleanToOut(ob)
      case rs: StringData     => stringToOut(rs)
      case id: IntData        => intToOut(id)
      case ri: LongData       => longToOut(ri)
      case uu: UuidData       => uuidToOut(uu)
      case dd: DateTimeData   => dateTimeToOut(dd)
      case fd: FloatData      => floatToOut(fd)
      case dd: DoubleData     => doubleToOut(dd)
      case bd: BigDecimalData => bigDecimalToOut(bd)
      case ba: ByteArrayData  => byteArrayToOut(ba)
      case ld: ListData[t]    => {
        val itemToOut = valueDefinition(ld.tDefinition)
        (input: List[t]) => {
          val listOfJson = input.map(itemToOut)
          toOutList(listOfJson)
        }
      }
      case EitherData(aDefinition, bDefinition) =>
        val aF = valueDefinition(aDefinition)
        val bF = valueDefinition(bDefinition)
        (input: A) =>
          {
            input match {
              case Left(aInput)  => aF(aInput)
              case Right(bInput) => bF(bInput)
            }
          }
      case e: EnumerationStringData[a] => enumerationToOut(e)
      case gd: KvpHListValue[h, hl] =>
        val fh = kvpHList(gd.kvpHList)
        (input: A) => fh(input.asInstanceOf[h])
      case x: HListConvert[h, hl, A] =>
        val fh = kvpHList(x.from)
        input: A =>
          {
            fh(x.fAtoH(input))
          }
      case s: SumTypeData[a, b] =>
        val fh = valueDefinition(s.from)
        input: A =>
          fh(s.fba(input))
    }

}
