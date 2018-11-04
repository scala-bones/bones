package com.bones.oas3

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZonedDateTime}
import java.util.UUID

import argonaut.Argonaut._
import argonaut._
import ArgonautScalaz._
import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.{InvalidValue, OptionalValidation, ValidValue, ValidationOp}


object ValidationOasInterpreter {
  case class OasObject(title: String)
  case class OasField()
  case class OasValidationProperty(propertyName: String, properyValue: Json)
}

case class ValidationToPropertyInterpreter() {

  import com.bones.validation.ValidationDefinition.{BigDecimalValidation => bdv, IntValidation => iv, StringValidation => sv}
  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_DATE_TIME

  private def toJson(a: Any): Option[Json] = a match {
    case str:String => Some(jString(str))
    case bd:BigDecimal => Some(jNumber(bd.asInstanceOf[BigDecimal]))
    case i:Int => Some(jNumber(i.asInstanceOf[Int]))
    case d:ZonedDateTime => Some(jString(dateTimeFormatter.format(d)))
    case _ => None

  }

  def apply[A](op: ValidationOp[A]): List[(Json.JsonField, Json)] = {
    op match {
      case OptionalValidation(required) => apply(required)
      case ValidValue(values) =>
        List("enum" -> jArray(values.flatMap(toJson).toList))
      case InvalidValue(values) => List("not" -> Json.obj("enum" -> jArray(values.flatMap(toJson).toList)))
      case sv.IsAlphanum() => List("matches" -> jString("^[:alnum:]+$"))
      case sv.MinLength(min) => List("minLength" -> jNumber(min))
      case sv.MaxLength(max) => List("maxLength" -> jNumber(max))
      case sv.MatchesRegex(r) => List("pattern" -> jString(r.toString))
      case sv.Length(l) => List("minLength" -> jNumber(l), "maxLength" -> jNumber(l))
      case sv.Custom(_,_,_) => List.empty
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

      case _ => List.empty

    }
  }
}


import argonaut._, Argonaut._

case class ValidationOasInterpreter(validationInterpreter: ValidationToPropertyInterpreter) {

  val propertiesLens = jObjectPL  >=>
    jsonObjectPL("properties") >=>
    jObjectPL


  def apply[A](fgo: ValueDefinitionOp[A]): JsonObject = fgo match {
    case op: OptionalValueDefinition[b] =>
      val data = apply(op.valueDefinitionOp)
      (data - "required") :+ ("required", jBool(false)) :+ ("nullable", jBool(true))
    case KvpNil =>
      Json(("type", jString("object")), ("properties", Json.jEmptyObject)).objectOrEmpty
    case op: KvpGroupHead[A,al, h, hl, t, tl] => {
      val head = apply(op.head)
      val headProperties = propertiesLens.get(Json.jObject(head)).getOrElse(JsonObject.empty)
      val tail = apply(op.tail)
      propertiesLens.mod(obj => JsonObject.fromTraversableOnce(headProperties.toList ::: obj.toList), Json.jObject(tail)).objectOrEmpty
//      JsonObject.fromTraversableOnce(apply(op.head).toMap.toList ::: apply(op.tail).toMap.toList)
    }
    case op: KvpSingleValueHead[h,t,tl,o,ol] => {
      val child = apply(op.fieldDefinition.op)
      val head = JsonObject.single(
        op.fieldDefinition.key.name,
        jObject(JsonObject.fromTraversableOnce(child.toList ::: op.validations.flatMap(x => validationInterpreter.apply(x))
      )))
      val tail = apply(op.tail)

      propertiesLens.mod(obj => JsonObject.fromTraversableOnce(head.toList ::: obj.toList), Json.jObject(tail)).objectOrEmpty
//      JsonObject.fromTraversableOnce(head.toList ::: tail.toList)

    }
    case _: BooleanData =>
      Json(
        "type" := "boolean",
        "required" := true,
        "example" := true
      ).objectOrEmpty
    case _: StringData =>
      Json(
        "type" := "string",
        "required" := true,
        "example" := "XYZ"
        ).objectOrEmpty
    case _: IntData =>
      Json(
        "type" := "integer",
        "required" := true,
        "example" :=  123
      ).objectOrEmpty
    case _: UuidData =>
      Json(
        "type" := "string",
        "required" := true,
        "example" := UUID.randomUUID().toString
      ).objectOrEmpty
    case DateData(format, _) =>
      Json(
        "type" := "date",
        "required" := true,
        "example" := format.format(LocalDateTime.now())
      ).objectOrEmpty
    case _: BigDecimalFromString =>
      Json(
        "type" := "double",
        "required" := true,
        "example" := "3.14"
      ).objectOrEmpty
    case _: DoubleData =>
      Json(
        "type" := "double",
        "required" := true,
        "example" := 3.14
      ).objectOrEmpty
    case ListData(definition) =>
      val items = apply(definition)
      Json(
        "type" := "array",
        "items" := jObject(items)
      ).objectOrEmpty
    case EitherData(aDefinition, bDefinition) =>
      val schemaA = apply(aDefinition)
      val schemaB = apply(bDefinition)

      Json(
        "oneOf" :=
          ("schema" := jObject(schemaA))
      ).objectOrEmpty
    case sumType: SumTypeData[a,b] => //(from, _, fba, keys, _) =>
      val fromOp = apply(sumType.from)

      //TODO: Create these based on the actual type, do not assume string
      val enums = sumType.keys.map(k => jString(k.toString))
      JsonObject.fromTraversableOnce(fromOp.toList :+ ("enum" := jArray(enums)))

//      Json(
//        "type" := "object",
//        "properties" := jObject(fromOp)
//      ).objectOrEmpty
    case EnumerationStringData(enumeration) =>
      Json(
        "type" := "string",
        "required" := true,
        "enum" := enumeration.values.map(_.toString).toList
      ).objectOrEmpty
    case EnumStringData(enum) =>
      Json(
        "type" := "string",
        "required" := true,
        "enum" := enum.map(_.toString)
      ).objectOrEmpty
    case transform: Convert[_,_] =>
      apply(transform.op)
//      JsonObject.single(convert.manifestOfA.runtimeClass.getSimpleName,
//        Json(
//          "type" := "object",
//          "properties" := jObject(apply(convert.op))
//        )
//      )
    case x => Json(x.toString := "needs impl").objectOrEmpty
  }
}
