package com.bones.oas3

import argonaut.Argonaut._
import argonaut.{Json, _}
import com.bones.crud.Algebra.{Create, CrudOp, Read}



case class CrudOasInterpreter() {

  val validationOasInterpreter = ValidationOasInterpreter(ValidationToPropertyInterpreter())

  def apply[A:Manifest](ops: List[CrudOp[A]]): String => Json = (urlPath: String) => {

    val (definitions, paths) = toJson(ops).apply(urlPath)

    val combinedDefinitions = definitions.values.map(_.objectOrEmpty)
      .fold(JsonObject.empty)( (a,b) => JsonObject.fromTraversableOnce(a.toMap ++ b.toMap))

    Json(
      "swagger" := "2.0",
      "info" := Json(
        "title" := ""
      ),
      "paths" := jObjectAssocList(paths),
      "definitions" := Json.jObject(combinedDefinitions)
    )

  }


  case class ToJsonResult(definitions: Map[Class[_], Json], paths: List[(JsonField, Json)])

  def toJson[A:Manifest](ops: List[CrudOp[A]]): String => (Map[String, Json], List[(JsonField, Json)]) = (urlPath: String) => {

    val runtimeClass = manifest[A].runtimeClass
    val definitionAndPaths = ops map {
      case op: Read[o] => {
        val definitions = Map(runtimeClass.getSimpleName -> jObject(validationOasInterpreter.apply(op.successSchemaForRead)))

        val path =  urlPath + "/{id}" :=
            Json("get" :=
              Json(
                "description" := s"Returns the ${runtimeClass.getSimpleName} of the given id",
                "parameters" := List(
                  (
                    "name" := "id",
                    "in" := "path",
                    "required" := true
                  )
                ),
                "produces" := Json.array(
                  jString("application/json")
                ),
                "responses" := Json(
                  "200" := Json(
                    "description" := s"The ${runtimeClass.getSimpleName} of the given id",
                    "schema" :=
                      Json(
                        "$ref" := s"#/definitions/${runtimeClass.getSimpleName}"
                      )
                  )
                )
              )
            )

        (definitions, path)

      }
      case op: Create[i,e,o] => {
        val path = urlPath :=
          Json("post" :=
            Json(
              "summary" := s"Creates a new ${runtimeClass.getSimpleName}",
              "consumes" := List("application/json"),
              "parameters" := List(
                Json(
                  "in" := "body",
                  "name" := s"${runtimeClass.getSimpleName}",
                  "schema" := Json(
                    "$ref" := s"#/definitions/${runtimeClass.getSimpleName}"
                    )
                )
              ),
              "produces" := List("application/json"),
              "responses" := (
                "200" := (
                  "description" := s"The ${runtimeClass.getSimpleName} of the given id",
                  "schema" :=
                    (
                      "$ref" := s"#/definitions/${runtimeClass.getSimpleName}"
                    )
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
