package com.bones.interpreter

import java.time.ZonedDateTime

import com.bones.data.Value._
import net.liftweb.json.JsonAST._


object EncodeToJValueInterpreter {
  type EncodeToJValue[A] = A => JValue
}


case class EncodeToJValueInterpreter() {
  import EncodeToJValueInterpreter._

  def apply[A](fgo: ValueDefinitionOp[A]): EncodeToJValue[A] =
    fgo match {
      case op: OptionalValueDefinition[b] => (input: A) => {
        input match {
          case Some(x) => apply(op.valueDefinitionOp).apply(x)
          case None => JNothing
        }
      }
      case KvpNil => (input: A) => JObject()
      case op: KvpGroupHead[A,l,h,hl,t,tl] => (input: A) => {
        val l = op.split(input)
        val m1 = this.apply(op.head).apply(l._1)
        val m2 = this.apply(op.tail).apply(l._2)
        JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
      }
      case op: KvpSingleValueHead[h,t,tl,A,ol] => (input: A) => {
        import shapeless.::
        val cast = input.asInstanceOf[h :: t]
        val val1 = apply(op.fieldDefinition.op).apply(cast.head)
        val m2 = this.apply(op.tail).apply(cast.tail).asInstanceOf[JObject]
        JObject(JField(op.fieldDefinition.key.name, val1) :: m2.obj)
      }
      case ob: BooleanData => (input: A) => JBool(input.asInstanceOf[Boolean])
      case rs: StringData => (input: A) => JString(input.asInstanceOf[String])
      case ri: IntData => (input: A) => JInt(input.asInstanceOf[Int])
      case uu: UuidData => (input: A) => JString(input.toString)
      case DateData(format, _) => (input: A) => JString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalFromString => (input: A) => JString(input.toString)
      case dd: DoubleData => (input: A) => JDouble(input.asInstanceOf[Double])
      case ListData(definition) => (input: A) => {
        val f = apply(definition)
        JArray(input.asInstanceOf[List[A]].map(i => f(i)))
      }
      case EitherData(aDefinition, bDefinition) => (input: A) => {
        input match {
          case Left(aInput) => apply(aDefinition)(aInput)
          case Right(bInput) => apply(bDefinition)(bInput)
        }
      }
      case SumTypeData(from, _, fba, _,  _) => (input: A) => {
        val encoder = apply(from)
        val outputValue = fba(input)
        encoder.apply(outputValue)
      }
      case EnumerationStringData(enumeration) => (input: A) => JString(input.toString)
      case EnumStringData(enum) => (input: A) => JString(input.toString)
      case Convert(op, _, fba) => (input: A) => {
        val b = fba(input)
        apply(op).apply(b)
      }

    }
}
