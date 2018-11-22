package com.bones.interpreter

import java.time.ZonedDateTime

import com.bones.data.Value._
import net.liftweb.json.JsonAST._
import shapeless.{HList, Nat}


object EncodeToJValueInterpreter {
  type EncodeToJValue[A] = A => JValue
}


case class EncodeToJValueInterpreter() {
  import EncodeToJValueInterpreter._

  def dataClass[A](dc: DataClass[A]): EncodeToJValue[A] = {
    dc match {
      case x: XMapData[a,al,b] =>
        (input: A) => kvpGroup(x.from).apply(x.fba(input))

      case op: OptionalDataClass[a] =>
        val dataF = dataClass(op.value)
        a: A => a match {
          case Some(x) => dataF.apply(x)
          case None => JNull
        }
    }
  }
  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): EncodeToJValue[H] =
    group match {
      case KvpNil => (input: H) => JObject()
      case op: KvpGroupHead[H,l,h,hl,t,tl] => (input: H) => {
        val l = op.split(input)
        val m1 = kvpGroup(op.head).apply(l._1)
        val m2 = kvpGroup(op.tail).apply(l._2)
        JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
      }
      case op: KvpSingleValueHead[h,t,tl,H] => (input: H) => {
        import shapeless.::
        val cast = input.asInstanceOf[h :: t]
        val val1 = valueDefinition(op.fieldDefinition.op).apply(cast.head)
        val m2 = kvpGroup(op.tail).apply(cast.tail).asInstanceOf[JObject]
        JObject(JField(op.fieldDefinition.key, val1) :: m2.obj)
      }
      case op: OptionalKvpGroup[h,hl] =>
        val oF = kvpGroup(op.kvpGroup)
        (input: H) => input.head match {
          case Some(x) => oF(x)
          case None => JNull
        }
      case op: KvpDataClassHead[h,t,tl,out] => {
        val dataF = dataClass(op.dataClass)
        val tailF = kvpGroup(op.tail)
        input: H => {
          val headJsonObj = dataF.apply(input.head)
          val tailJsonObj = tailF.apply(input.tail)
          JObject(headJsonObj.asInstanceOf[JObject].obj ::: tailJsonObj.asInstanceOf[JObject].obj)
        }
      }
    }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => (input: A) => {
        input match {
          case Some(x) => valueDefinition(op.valueDefinitionOp).apply(x)
          case None => JNothing
        }
      }
      case ob: BooleanData => (input: A) => JBool(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => JString(input.asInstanceOf[String])
      case ri: LongData => (input: A) => JInt(BigInt(input.asInstanceOf[Long]))
      case uu: UuidData => (input: A) => JString(input.toString)
      case DateTimeData(format, _, _) => (input: A) => JString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalData => (input: A) => JString(input.toString)
      case ListData(definition, _) => (input: A) => {
        val f = valueDefinition(definition)
        JArray(input.asInstanceOf[List[A]].map(i => f(i)))
      }
      case EitherData(aDefinition, bDefinition) => (input: A) => {
        input match {
          case Left(aInput) => valueDefinition(aDefinition)(aInput)
          case Right(bInput) => valueDefinition(bDefinition)(bInput)
        }
      }
      case EnumerationStringData(enumeration, _) => (input: A) => JString(input.toString)
      case EnumStringData(enum, _) => (input: A) => JString(input.toString)
      case SumTypeData(op, fab, fba, keys, validations) => (input: A) => {
        val a = fba(input)
        valueDefinition(op).apply(a)
      }
      case op: KvpGroupData[h,hl] => {
        val fg = kvpGroup(op.kvpGroup)
        (input: A) => {
          fg(input.asInstanceOf[h])
        }
      }

    }
}
