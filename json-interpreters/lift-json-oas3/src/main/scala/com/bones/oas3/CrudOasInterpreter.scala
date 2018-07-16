package com.bones.oas3

import argonaut.Json
import com.bones.crud.Algebra.{Create, CrudOp, Read}
import com.bones.validation.ValidationDefinition.ValidationOp
import argonaut.Argonaut._
import argonaut._
import com.bones.data.Algebra.DataDefinitionOp

case class CrudOasInterpreter() {

  val validationOasInterpreter = ValidationOasInterpreter(ValidationToPropertyInterpreter())

  import CrudOasInterpreter._

  def apply[A:Manifest](ops: List[CrudOp[A]]): String => Json = (urlPath: String) => {

    val (definitions, paths) = toJson(ops).apply(urlPath)

    val combinedDefinitions = definitions.values.map(_.objectOrEmpty)
      .fold(JsonObject.empty)( (a,b) => JsonObject.fromTraversableOnce(a.toMap ++ b.toMap))

    Json(
      "swagger" := "2.0",
      "paths" := jObjectAssocList(paths),
      "definitions" := Json.jObject(combinedDefinitions)
    )

  }


  case class ToJsonResult(definitions: Map[Class[_], Json], paths: List[(JsonField, Json)])

  def toJson[A:Manifest](ops: List[CrudOp[A]]): String => (Map[String, Json], List[(JsonField, Json)]) = (urlPath: String) => {

    val definitionAndPaths = ops map {
      case op: Read[o] => {
        val runtimeClass = manifest[A].runtimeClass
        val definitions = Map(runtimeClass.getSimpleName -> jObject(validationOasInterpreter.apply(op.successSchemaForRead)))

        val path =  urlPath + "/{id}" :=
            Json("get" :=
              Json(
                "description" := "Returns a X given the id",
                "parameters" := Json.array(
                  Json(
                    "name" := "id",
                    "in" := "path",
                    "required" := "true",
                    "schema" :=
                      Json("$ref" := s"#/definitions/${runtimeClass.getCanonicalName}")
                  )
                )
              )
            )

        (definitions, path)

      }
      case op: Create[i,e,o] => {
        val path = urlPath :=
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
        (Map.empty, path)
      }
    }

    //combine the definitions to avoid duplicates.
    val definitions = definitionAndPaths.flatMap(_._1.toList).toMap
    (definitions, definitionAndPaths.map(_._2))
  }
}
