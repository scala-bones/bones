package com.bones.interpreter

import java.time.{LocalDate, ZonedDateTime}
import java.util.Date

import com.bones.data.Algebra._
import com.bones.data.HListAlgebra._
import net.liftweb.json.JsonAST._
import shapeless.HNil


object EncodeToJValueInterpreter {
  type EncodeToJValue[A] = A => JValue
}


case class EncodeToJValueInterpreter() {
  import EncodeToJValueInterpreter._

  def apply[A](fgo: DataDefinitionOp[A]): EncodeToJValue[A] = (input: A) =>
    fgo match {
      case op: OptionalDataDefinition[b] => {
        input match {
          case Some(x) => apply(op.dataDefinitionOp).apply(x)
          case None => JNothing
        }
      }
      case op: HListPrependN[A,p,s] => {
          val l = op.split(input)
          val m1 = this.apply(op.prefix).apply(l.head)
          val m2 = this.apply(op.suffix).apply(l.tail.head)
          JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
      }
      case op: HMember[a] => {
        import shapeless.::
        val res1 = this(op.op1.op)(input.asInstanceOf[a :: HNil].head)
        JObject(List(JField(op.op1.key.name, res1)))
      }
      case ob: BooleanData => JBool(input.asInstanceOf[Boolean])
      case rs: StringData => JString(input.asInstanceOf[String])
      case ri: IntData => JInt(input.asInstanceOf[Int])
      case uu: UuidData => JString(input.toString)
      case DateData(format, _) => JString(format.format(input.asInstanceOf[ZonedDateTime]))
      case bd: BigDecimalFromString => JString(input.toString)
      case dd: DoubleData => JDouble(input.asInstanceOf[Double])
      case ListData(definition) => {
        val f = apply(definition)
        JArray(input.asInstanceOf[List[A]].map(i => f(i)))
      }
      case EitherData(aDefinition, bDefinition) => {
        input match {
          case Left(aInput) => apply(aDefinition)(aInput)
          case Right(bInput) => apply(bDefinition)(bInput)
        }
      }
      case ConversionData(from, _, fba, _) => {
        val encoder = apply(from)
        val outputValue = fba(input)
        encoder.apply(outputValue)
      }
      case EnumerationStringData(enumeration) => JString(input.toString)
      case EnumStringData(enum) => JString(input.toString)
      case Transform(op, fab, _) => {
        val b = fab(input)
        apply(op).apply(b)
      }

    }
}
