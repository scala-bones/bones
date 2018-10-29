package com.bones.argonaut

import java.time.ZonedDateTime

import argonaut._
import com.bones.data.Value._
object EncodeToArgonautInterpreter {

  type EncodeToJValue[A] = A => Json
}
class EncodeToArgonautInterpreter {

  import EncodeToArgonautInterpreter._

  def apply[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => (input: A) => {
        input match {
          case Some(x) => apply(op.valueDefinitionOp).apply(x)
          case None => Json.jNull
        }
      }
      case KvpNil => (input: A) => Json.obj()
      case op: KvpGroupHead[A,l,h,hl,t,tl] => (input: A) => {
        val l = op.split(input)
        val m1 = this.apply(op.head).apply(l._1)
        val m2 = this.apply(op.tail).apply(l._2)
        val values1 = m1.obj.toList.flatMap(_.toList)
        val values2 = m2.obj.toList.flatMap(_.toList)
        Json.obj( values1 ::: values2 :_*)

      }
      case op: KvpSingleValueHead[h,t,tl,A,ol] => (input: A) => {
        import shapeless.::
        val cast = input.asInstanceOf[h :: t]
        val val1 = apply(op.fieldDefinition.op).apply(cast.head)

        val values = this.apply(op.tail).apply(cast.tail).obj.toList.flatMap(_.toList)
        Json.obj( (op.fieldDefinition.key.name, val1) :: values :_* )
      }
      case ob: BooleanData => (input: A) => Json.jBool(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => Json.jString(input.asInstanceOf[String])
      case ri: IntData => (input: A) => Json.jNumber(input.asInstanceOf[Int].toLong)
      case uu: UuidData => (input: A) => Json.jString(input.toString)
      case DateData(format, _) => (input: A) => Json.jString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalFromString => (input: A) => Json.jString(input.toString)
      case dd: DoubleData => (input: A) =>
        Json.jNumber(input.asInstanceOf[Double])
      case ListData(definition) => (input: A) => {
        val f = apply(definition)
        Json.array(input.asInstanceOf[List[A]].map(i => f(i)) :_*)
      }
      case EitherData(aDefinition, bDefinition) => (input: A) => {
        input match {
          case Left(aInput) => apply(aDefinition)(aInput)
          case Right(bInput) => apply(bDefinition)(bInput)
        }
      }
      case SumTypeData(from, _, fba, _, _) => (input: A) => {
        val encoder = apply(from)
        val outputValue = fba(input)
        encoder.apply(outputValue)
      }
      case EnumerationStringData(enumeration) => (input: A) => Json.jString(input.toString)
      case EnumStringData(enum) => (input: A) => Json.jString(input.toString)
      case Transform(op, fab, _) => (input: A) => {
        val b = fab(input)
        apply(op).apply(b)
      }
      case ByteReferenceData() => ???

    }
}
