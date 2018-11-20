package com.bones.argonaut

import java.time.ZonedDateTime

import argonaut._
import com.bones.data.Value._
import shapeless.{HList, HNil, Nat}
object EncodeToArgonautInterpreter {

  type EncodeToJValue[A] = A => Json
}
class EncodeToArgonautInterpreter {

  import EncodeToArgonautInterpreter._

  def kvpGroup[H<:HList, HL<:Nat](group: KvpGroup[H,HL]) : EncodeToJValue[H] = {
    group match {
      case KvpNil => (input: H) => Json.obj()
      case op: KvpGroupHead[a,l,h,hl,t,tl] => {
        val fh = kvpGroup[h,hl](op.head)
        val ft = kvpGroup[t,tl](op.tail)
        (input: H) => {
          val split = op.split(input)
          val values1 = fh(split._1).obj.toList.flatMap(_.toList)
          val values2 = ft(split._2).obj.toList.flatMap(_.toList)
          Json.obj( values1 ::: values2 :_*)
        }
      }
      case op: KvpSingleValueHead[h,t,tl,H,ol] => (input: H) => {
        import shapeless.::
        val cast = input.asInstanceOf[h :: t]
        val val1 = valueDefinition(op.fieldDefinition.op).apply(cast.head)

        val values = kvpGroup(op.tail).apply(cast.tail).obj.toList.flatMap(_.toList)
        Json.obj( (op.fieldDefinition.key, val1) :: values :_* )
      }
      case XMapData(op, fab, fba, _) => (input: H) => {
        kvpGroup(op).apply(input)
      }

    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => (input: A) => {
        input match {
          case Some(x) => valueDefinition(op.valueDefinitionOp).apply(x)
          case None => Json.jNull
        }
      }

      case ob: BooleanData => (input: A) => Json.jBool(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => Json.jString(input.asInstanceOf[String])
      case ri: IntData => (input: A) => Json.jNumber(input.asInstanceOf[Int].toLong)
      case uu: UuidData => (input: A) => Json.jString(input.toString)
      case DateData(format, _, _) => (input: A) => Json.jString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalFromString => (input: A) => Json.jString(input.toString)
      case dd: DoubleData => (input: A) =>
        Json.jNumber(input.asInstanceOf[Double])
      case ListData(definition, _) => (input: A) => {
        val f = valueDefinition(definition)
        Json.array(input.asInstanceOf[List[A]].map(i => f(i)) :_*)
      }
      case EitherData(aDefinition, bDefinition) => (input: A) => {
        input match {
          case Left(aInput) => valueDefinition(aDefinition)(aInput)
          case Right(bInput) => valueDefinition(bDefinition)(bInput)
        }
      }
      case EnumerationStringData(enumeration, _) => (input: A) => Json.jString(input.toString)
      case EnumStringData(enum, _) => (input: A) => Json.jString(input.toString)
      case ByteReferenceData(_) => ???

    }
}
