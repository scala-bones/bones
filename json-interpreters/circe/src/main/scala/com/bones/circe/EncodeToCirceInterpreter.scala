package com.bones.circe

import java.time.ZonedDateTime

import com.bones.data.Value._
import io.circe._
import shapeless.{HList, HNil, Nat}

object EncodeToCirceInterpreter {

  type EncodeToJValue[A] = A => Json

  def dataClass[A](dc: DataClass[A]): EncodeToJValue[A] = {
    dc match {
      case x: XMapData[a,al,b] =>
        val groupF = kvpGroup(x.from)
        input: A => groupF.apply(x.fba(input))
      case op: OptionalDataClass[a] =>
        val dataClassF = dataClass(op.value)
        a: A => a match {
          case Some(x) => dataClassF.apply(x)
          case None => Json.Null
        }
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): EncodeToJValue[H] =
    group match {
      case KvpNil => (input: H) => Json.obj()
      case op: KvpGroupHead[out,l,h,hl,t,tl] =>
        val headF = kvpGroup(op.head)
        val tailF = kvpGroup[t,tl](op.tail)
        (input: H) => {
          val l = op.split(input)
          val values1 = headF.apply(l._1).asObject.toList.flatMap(_.toList)
          val values2 = tailF.apply(l._2).asObject.toList.flatMap(_.toList)
          Json.obj( (values1 ::: values2) :_*)
        }
      case op: KvpSingleValueHead[h,t,tl,H] =>
        val valueF = valueDefinition(op.fieldDefinition.op)
        (input: H) => {
          import shapeless.::
          val cast = input.asInstanceOf[h :: t]
          val val1 = valueF.apply(cast.head)
          val values = kvpGroup(op.tail)(cast.tail).asObject.toList.flatMap(_.toList)
          Json.obj( ( (op.fieldDefinition.key, val1) :: values) :_* )
        }
      case op: KvpDataClassHead[h,t,tl,o] => {
        val hF = dataClass(op.dataClass)
        val tailF = kvpGroup(op.tail)
        (input: H) => {
          val hFields = hF(input.head).asObject.toList.flatMap(_.toList)
          val tailFields = tailF(input.tail).asObject.toList.flatMap(_.toList)
          Json.obj( (hFields ::: tailFields) :_*)
        }
      }
      case op: OptionalKvpGroup[h,hl] =>
        val oF = kvpGroup(op.kvpGroup)
        input: H => input.head match {
          case Some(kvp) => oF(kvp)
          case None => Json.Null
        }
    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] =>
        val valueF = valueDefinition(op.valueDefinitionOp)
        (input: A) => {
          input match {
            case Some(x) => valueF(x)
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
      case EitherData(aDefinition, bDefinition) =>
        val aF = valueDefinition(aDefinition)
        val bF = valueDefinition(bDefinition)
        (input: A) => {
          input match {
            case Left(aInput) => aF(aInput)
            case Right(bInput) => bF(bInput)
          }
        }
      case EnumerationStringData(enumeration, validations) => (input: A) => Json.fromString(input.toString)
      case EnumStringData(enum, validations) => (input: A) => Json.fromString(input.toString)
      case gd: KvpGroupData[h,hl] => {
        val fh = kvpGroup(gd.kvpGroup)
        input: A => fh(input.asInstanceOf[h])
      }
    }
}
