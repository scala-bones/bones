package com.bones.oas3

import com.bones.data.Algebra._
import com.bones.data.HListAlgebra.{HListPrependN, HMember}
import net.liftweb.json.JsonAST.{JBool, JField, JObject, JString}

class ToOasInterpreter {
  def apply[A](fgo: DataDefinitionOp[A]): JObject = fgo match {
    case op: OptionalDataDefinition[b] => {
      //need to remove "required" field
      JObject(apply(op.dataDefinitionOp).obj :+ JField("nullable", JBool(true)))
    }
    case op: HListPrependN[A, p, s] => {
      ???
    }
    case op: HMember[a] => ???
    case ob: BooleanData => JObject(List(JField("type", JString("boolean")), JField("required", JBool(true))))
    case rs: StringData => JObject(List(JField("type", JString("string")), JField("required", JBool(true))))
    case ri: IntData => JObject(List(JField("type", JString("int")), JField("required", JBool(true))))
    case uu: UuidData => JObject(List(JField("type", JString("string")), JField("required", JBool(true))))
    case DateData(format, _) => JObject(List(JField("type", JString("date")), JField("required", JBool(true))))
    case bd: BigDecimalFromString => JObject(List(JField("type", JString("double")), JField("required", JBool(true))))
    case dd: DoubleData => JObject(List(JField("type", JString("double")), JField("required", JBool(true))))
    case ListData(definition) => ???
    case EitherData(aDefinition, bDefinition) => ???
    case ConversionData(from, _, fba, _) => ???
    case EnumerationStringData(enumeration) => JObject(List(JField("type", JString("string")), JField("required", JBool(true))))
    case EnumStringData(enum) => JObject(List(JField("type", JString("string")), JField("required", JBool(true))))
    case Transform(op, fab, _) => ???
  }
}
