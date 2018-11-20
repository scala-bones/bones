package com.bones.circe

import java.time.ZonedDateTime

import com.bones.data.Value._
import io.circe._
import shapeless.{HList, HNil, Nat}

object EncodeToCirceInterpreter {

  type EncodeToJValue[A] = A => Json

  def value[A](value: Value[A]): EncodeToJValue[A] = {
    value match {
      case x: XMapData[_,_,A] =>
        (a: A) => kvpGroup(x).apply(a :: HNil)
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): EncodeToJValue[H] =
    group match {
      case KvpNil => (input: H) => Json.obj()
      case op: KvpGroupHead[H,l,h,hl,t,tl] => (input: H) => {
        val l = op.split(input)
        val m1 = kvpGroup(op.head).apply(l._1)
        val m2 = kvpGroup(op.tail).apply(l._2)
        val values1 = m1.asObject.toList.flatMap(_.toList)
        val values2 = m2.asObject.toList.flatMap(_.toList)
        Json.obj( (values1 ::: values2) :_*)

      }
      case op: KvpSingleValueHead[h,t,tl,H,ol] => (input: H) => {
        import shapeless.::
        val cast = input.asInstanceOf[h :: t]
        val val1 = valueDefinition(op.fieldDefinition.op).apply(cast.head)

        val values = kvpGroup(op.tail)(cast.tail).asObject.toList.flatMap(_.toList)
        Json.obj( ( (op.fieldDefinition.key, val1) :: values) :_* )
      }
      case XMapData(op, _, fba, validations) => (input: H) => {
        val b = fba(input)
        kvpGroup(op).apply(b)
      }


    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => (input: A) => {
        input match {
          case Some(x) => valueDefinition(op.valueDefinitionOp).apply(x)
          case None => Json.Null
        }
      }
      case ob: BooleanData => (input: A) => Json.fromBoolean(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => Json.fromString(input.asInstanceOf[String])
      case ri: IntData => (input: A) => Json.fromInt(input.asInstanceOf[Int])
      case uu: UuidData => (input: A) => Json.fromString(input.toString)
      case DateData(format, _, validations) => (input: A) => Json.fromString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalFromString => (input: A) => Json.fromString(input.toString)
      case dd: DoubleData => (input: A) => {
        Json.fromDouble(input.asInstanceOf[Double]) match {
          case Some(d) => d
          case None => Json.fromString("NaN")
        }
      }
      case ListData(definition, validations) => (input: A) => {
        val f = valueDefinition(definition)
        Json.arr(input.asInstanceOf[List[A]].map(i => f(i)) :_*)
      }
      case EitherData(aDefinition, bDefinition) => (input: A) => {
        input match {
          case Left(aInput) => valueDefinition(aDefinition)(aInput)
          case Right(bInput) => valueDefinition(bDefinition)(bInput)
        }
      }
      case EnumerationStringData(enumeration, validations) => (input: A) => Json.fromString(input.toString)
      case EnumStringData(enum, validations) => (input: A) => Json.fromString(input.toString)
      case gd: KvpGroupData[h,hl] => {
        val fh = kvpGroup(gd.kvpGroup)
        (input: A) => fh(input.asInstanceOf[h])
      }
    }
}
