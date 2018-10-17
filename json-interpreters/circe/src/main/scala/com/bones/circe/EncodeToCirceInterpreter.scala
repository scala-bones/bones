package com.bones.circe

import java.time.ZonedDateTime

import com.bones.circe.EncodeToCirceInterpreter.EncodeToJValue
import com.bones.data.Value._
import io.circe._
import io.circe.parser._

object EncodeToCirceInterpreter {

  type EncodeToJValue[A] = A => Json
}
case class EncodeToCirceInterpreter() {

  def apply[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => (input: A) => {
        input match {
          case Some(x) => apply(op.valueDefinitionOp).apply(x)
          case None => Json.Null
        }
      }
      case KvpNil => (input: A) => Json.obj()
      case op: KvpGroupHead[A,l,h,hl,t,tl] => (input: A) => {
        val l = op.split(input)
        val m1 = this.apply(op.head).apply(l._1)
        val m2 = this.apply(op.tail).apply(l._2)
        val values1 = m1.asObject.toList.flatMap(_.toList)
        val values2 = m2.asObject.toList.flatMap(_.toList)
        Json.obj( (values1 ::: values2) :_*)

      }
      case op: KvpSingleValueHead[h,t,tl,A,ol] => (input: A) => {
        import shapeless.::
        val cast = input.asInstanceOf[h :: t]
        val val1 = apply(op.fieldDefinition.op).apply(cast.head)

        val values = this.apply(op.tail).apply(cast.tail).asObject.toList.flatMap(_.toList)
        Json.obj( ( (op.fieldDefinition.key.name, val1) :: values) :_* )
      }
      case ob: BooleanData => (input: A) => Json.fromBoolean(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => Json.fromString(input.asInstanceOf[String])
      case ri: IntData => (input: A) => Json.fromInt(input.asInstanceOf[Int])
      case uu: UuidData => (input: A) => Json.fromString(input.toString)
      case DateData(format, _) => (input: A) => Json.fromString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalFromString => (input: A) => Json.fromString(input.toString)
      case dd: DoubleData => (input: A) => {
        Json.fromDouble(input.asInstanceOf[Double]) match {
          case Some(d) => d
          case None => Json.fromString("NaN")
        }
      }
      case ListData(definition) => (input: A) => {
        val f = apply(definition)
        Json.arr(input.asInstanceOf[List[A]].map(i => f(i)) :_*)
      }
      case EitherData(aDefinition, bDefinition) => (input: A) => {
        input match {
          case Left(aInput) => apply(aDefinition)(aInput)
          case Right(bInput) => apply(bDefinition)(bInput)
        }
      }
      case ConversionData(from, _, fba, _) => (input: A) => {
        val encoder = apply(from)
        val outputValue = fba(input)
        encoder.apply(outputValue)
      }
      case EnumerationStringData(enumeration) => (input: A) => Json.fromString(input.toString)
      case EnumStringData(enum) => (input: A) => Json.fromString(input.toString)
      case Transform(op, fab, _) => (input: A) => {
        val b = fab(input)
        apply(op).apply(b)
      }

    }
}
