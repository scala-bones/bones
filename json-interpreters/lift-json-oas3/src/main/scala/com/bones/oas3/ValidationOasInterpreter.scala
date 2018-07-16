package com.bones.oas3

import java.time.{LocalDateTime, ZonedDateTime}
import java.time.format.DateTimeFormatter
import java.util.UUID

import argonaut.Argonaut._
import argonaut._
import com.bones.data.Algebra._
import com.bones.data.HListAlgebra.{HListPrependN, HMember}
import com.bones.validation.ValidationDefinition.{InvalidValue, OptionalValidation, ValidValue, ValidationOp}


object ValidationOasInterpreter {



  case class OasObject(title: String)
  case class OasField()
  case class OasValidationProperty(propertyName: String, properyValue: Json)

}

case class ValidationToPropertyInterpreter() {

  import com.bones.validation.ValidationDefinition.{BigDecimalValidation => bdv, IntValidation => iv, StringValidation => sv}
  val dateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME

  private def toJson(a: Any): Option[Json] = a match {
    case str:String => Some(jString(str))
    case bd:BigDecimal => Some(jNumber(bd.asInstanceOf[BigDecimal]))
    case i:Int => Some(jNumber(i.asInstanceOf[Int]))
    case d:ZonedDateTime => Some(jString(dateTimeFormatter.format(d)))
    case _ => None

  }

  def apply[A](op: ValidationOp[A]): List[(Json.JsonField, Json)] = {
    op match {
      case OptionalValidation(required) => {
        apply(required)
      }
      case ValidValue(values) => {
        List("enum" -> jArray(values.flatMap(toJson).toList))
      }
      case InvalidValue(values) => List("not" -> Json.obj("enum" -> jArray(values.flatMap(toJson).toList)))
      case sv.IsAlphanum() => List("matches" -> jString("^[:alnum:]+$"))
      case sv.MinLength(min) => List("minLength" -> jNumber(min))
      case sv.MaxLength(max) => List("maxLength" -> jNumber(max))
      case sv.MatchesRegex(r) => List("pattern" -> jString(r.toString))
      case sv.Length(l) => List("minLength" -> jNumber(l), "maxLength" -> jNumber(l))
      case sv.Custom(r,err,description) => List.empty
      case sv.Guid() =>
        List("minLength" -> jNumber(36),
             "maxLength" -> jNumber(36),
             "pattern" -> jString("(^([0-9A-Fa-f]{8}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{12})$)"))
      case sv.Uppercase() => List.empty
      case sv.CreditCard() => List.empty
      case sv.Token() => List.empty
      case sv.Email() => List("format" -> jString("email"))
      case sv.Hex() => List.empty
      case sv.Base64() => List.empty
      case sv.Hostname() => List("format" -> jString("hostname"))
      case sv.Ipv4() => List("format" -> jString("ipv4"))
      case sv.Lowercase() => List.empty
      case sv.Uri() => List("format" -> jString("uri"))

      case iv.Between(min, max) => List("exclusiveMinimum" -> jNumber(min), "exclusiveMaximum" -> jNumber(max))
      case iv.Greater(gt) => List("minimum" -> jNumber(gt))
      case iv.Less(lt) => List("maximum" -> jNumber(lt))
      case iv.Max(max) => List("maximum" -> jNumber(max))
      case iv.Min(min) => List("minimum" -> jNumber(min))
      case iv.Multiple(x) => List("multipleOf" -> jNumber(x))
      case iv.Negative() => List("exclusiveMaximum" -> jNumber(0))
      case iv.Positive() => List("exclusiveMinimum" -> jNumber(0))

      case bdv.Max(max) => List("maximum" -> jNumber(max))
      case bdv.Min(min) => List("minimum" -> jNumber(min))

    }
  }
}

case class ValidationOasInterpreter(validationInterpreter: ValidationToPropertyInterpreter) {
  def apply[A](fgo: DataDefinitionOp[A]): JsonObject = fgo match {
    case op: OptionalDataDefinition[b] => {
      //need to remove "required" field
      apply(op.dataDefinitionOp) :+ ("required", jBool(false)) :+ ("nullable", jBool(true))
    }
    case op: HListPrependN[A, p, s] => {
      JsonObject.fromTraversableOnce(apply(op.prefix).toMap.toList ::: apply(op.suffix).toMap.toList)
    }
    case op: HMember[a] => {
      val child = apply(op.op1.op)
      JsonObject.single(op.op1.key.name,
        jObject(JsonObject.fromTraversableOnce(child.toMap.toList :::
        op.validations.flatMap(x => validationInterpreter.apply(x))))
      )
    }
    case ob: BooleanData => JsonObject.single("type", jString("boolean")) :+ ("required", jBool(true)) :+ ("example", jBool(true))
    case rs: StringData => JsonObject.single("type", jString("string")) :+ ("required", jBool(true)) :+ ("example", jString("123"))
    case ri: IntData => JsonObject.single("type", jString("integer")) :+ ("required", jBool(true)) :+ ("example", jNumber(123))
    case uu: UuidData => JsonObject.single("type", jString("string")) :+ ("required", jBool(true)) :+ ("example", jString(UUID.randomUUID().toString))
    case DateData(format, _) => JsonObject.single("type", jString("date")) :+ ("required", jBool(true)) :+ ("example", jString(format.format(new LocalDateTime)))
    case bd: BigDecimalFromString => JsonObject.single("type", jString("double")) :+ ("required", jBool(true)) :+ ("example", jString("3.14"))
    case dd: DoubleData => JsonObject.single("type", jString("double")) :+ ("required", jBool(true)) :+ ("example", jNumber(3.14))
    case ListData(definition) => {
      val items = apply(definition)
      ("type", jString("array")) +: ("items", jObject(items)) +: JsonObject.empty
    }
    case EitherData(aDefinition, bDefinition) => {
      val schemaA = apply(aDefinition)
      val schemaB = apply(bDefinition)

      JsonObject.single("oneOf", Json("schema" -> jObject(schemaA)))
    }
    case ConversionData(from, _, fba, _) => {
      val fromOp = apply(from)
      ("type" -> jString("object")) +: JsonObject.single("properties", jObject(fromOp))
    }
    case EnumerationStringData(enumeration) => JsonObject.single("type", jString("string")) :+ ("required" -> jBool(true))
    case EnumStringData(enum) => JsonObject.single("type", jString("string")) :+ ("required", jBool(true))
    case transform: Transform[_,_] => {
      JsonObject.single(transform.manifestOfA.runtimeClass.getSimpleName,
        jObject(("type" -> jString("object")) +: JsonObject.single("properties", jObject(apply(transform.op))))
      )
    }
    case Check(obj, check) => JsonObject.single("check", jString("needs impl"))
    case x => JsonObject.single(x.toString, jString("needs impl"))
  }
}
