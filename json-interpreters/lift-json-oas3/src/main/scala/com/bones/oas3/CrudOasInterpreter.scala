package com.bones.oas3

import com.bones.crud.Algebra._
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media.IntegerSchema
import io.swagger.v3.oas.models.parameters.Parameter
import io.swagger.v3.oas.models.responses.{ApiResponse, ApiResponses}



object CrudOasInterpreter {


  def toSwaggerCore[A:Manifest](ops: List[CrudOp[A]], urlPath: String): OpenAPI = {
    val entityName = manifest[A].runtimeClass.getSimpleName

    val openAPI = new OpenAPI()
    val withAddedPaths = ops.foreach {
      case op: Read[o] =>
        val outputSchema = SwaggerCoreInterpreter(op.successSchemaForRead)
        val components = new Components()
          .addSchemas(entityName, outputSchema)
        openAPI.components(components)

        val apiResponse = new ApiResponse()
          .$ref(s"#/components/schemas/${entityName}")
        val apiResponses = new ApiResponses()
          .addApiResponse("200", apiResponse)

        val paramSchema = new IntegerSchema()
        val param = new Parameter()
          .name("id").in("path").required(true)
          .description(s"id of the ${entityName} to retrieve")
          .schema(paramSchema)

        val operation = new Operation()
          .responses(apiResponses)
          .parameters(java.util.Collections.singletonList(param))
          .tags(java.util.Collections.singletonList(entityName))
          .summary(s"Find ${entityName} by ID")
          .description(s"Returns ${entityName} by id")
          .operationId(s"get${entityName}ById")
        val pathItem = new PathItem()
          .get(operation)

        openAPI.path(urlPath + "/{id}", pathItem)

      case _ =>
    }
    openAPI
  }


//  case class ToJsonResult(definitions: Map[Class[_], Json], paths: List[(JsonField, Json)])

//  def toJson[A:Manifest](ops: List[CrudOp[A]]): (Map[String, Json], List[(JsonField, Json)]) => {
//
//    val runtimeClass = manifest[A].runtimeClass
//    val definitionAndPaths = ops map {
//      case from: Read[o] => {
//        val definitions = Map(runtimeClass.getSimpleName -> jObject(SwaggerCoreInterpreter(from.successSchemaForRead)))
//
//        val path =  urlPath + "/{id}" :=
//            Json("get" :=
//              Json(
//                "description" := s"Returns the ${runtimeClass.getSimpleName} of the given id",
//                "parameters" := List(
//                  (
//                    "entityName" := "id",
//                    "in" := "path",
//                    "required" := true
//                  )
//                ),
//                "produces" := Json.array(
//                  jString("application/json")
//                ),
//                "responses" := Json(
//                  "200" := Json(
//                    "description" := s"The ${runtimeClass.getSimpleName} of the given id",
//                    "schema" :=
//                      Json(
//                        "$ref" := s"#/definitions/${runtimeClass.getSimpleName}"
//                      )
//                  )
//                )
//              )
//            )
//
//        (definitions, path)
//
//      }
//      case from: Create[i,e,o] => {
//        val path = urlPath :=
//          Json("post" :=
//            Json(
//              "summary" := s"Creates a new ${runtimeClass.getSimpleName}",
//              "consumes" := List("application/json"),
//              "parameters" := List(
//                Json(
//                  "in" := "body",
//                  "entityName" := s"${runtimeClass.getSimpleName}",
//                  "schema" := Json(
//                    "$ref" := s"#/definitions/${runtimeClass.getSimpleName}"
//                    )
//                )
//              ),
//              "produces" := List("application/json"),
//              "responses" := (
//                "200" := (
//                  "description" := s"The ${runtimeClass.getSimpleName} of the given id",
//                  "schema" :=
//                    (
//                      "$ref" := s"#/definitions/${runtimeClass.getSimpleName}"
//                    )
//                )
//              )
//            )
//          )
//        (Map.empty, path)
//      }
//      case from: Update[i,e,o] => {
//        val path = urlPath :=
//          (Json("put" := true))
//        (Map.empty, path)
//      }
//      case from: Delete[o] => {
//        val path = urlPath :=
//          (Json("put" := true))
//        (Map.empty, path)
//      }
//    }
//
//    //combine the definitions to avoid duplicates.
//    val definitions = definitionAndPaths.flatMap(_._1.toList).toMap
//    (definitions, definitionAndPaths.map(_._2))
//  }
}
