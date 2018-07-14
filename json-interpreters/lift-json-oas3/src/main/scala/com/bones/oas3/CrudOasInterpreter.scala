package com.bones.oas3

import argonaut.Json
import com.bones.rest.Algebra.{Create, CrudOp, Read}
import com.bones.validation.ValidationDefinition.ValidationOp
import argonaut.Argonaut._
import argonaut._


object CrudOasInterpreter {
  type URL = String
  type PluralURL = String
}
case class CrudOasInterpreter() {

  val validationOasInterpreter = ValidationOasInterpreter(ValidationToPropertyInterpreter())

  import CrudOasInterpreter._

  def apply[A](ops: List[CrudOp[A]]): (URL, PluralURL) => Json = (url: URL, plural: PluralURL) => {

    Json(
      "swagger" := "2.0",
      "paths" := jObjectAssocList(toJson(ops).apply(url, plural))
    )
  }

  def toJson[A](ops: List[CrudOp[A]]): (URL, PluralURL) => List[(JsonField, Json)] = (url: URL, plural: PluralURL) => {
    ops map {
      case op: Read[o] => {
          url + "/{id}" :=
            Json("get" :=
              Json(
                "description" := "Returns a X by id",
                "parameters" := Json.array(
                  Json(
                    "name" := "userId",
                    "in" := "path",
                    "required" := "true",
                    "schema" := jObject(validationOasInterpreter.apply(op.successSchemaForRead))
                  )
                )
              )
            )

      }
      case op: Create[i,e,o] => {
        plural :=
          Json("post" :=
            Json("description" := "Creates a X",
              "requestBody" :=
                Json(
                  "required" := true,
                  "content" :=
                    Json(
                      "application/json" :=
                      Json("schema" := jObject(validationOasInterpreter.apply(op.successSchemaForCreate)))
                    )
                )
            )
          )
      }
    }
  }
}
