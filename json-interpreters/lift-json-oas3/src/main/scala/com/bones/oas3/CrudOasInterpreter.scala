package com.bones.oas3

import com.bones.crud.Algebra._
import com.bones.data.Value.DataClass
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.{ApiResponse, ApiResponses}
import scala.collection.JavaConverters._



object CrudOasInterpreter {



  private def upsertComponent[A](openApi: OpenAPI, entityName: String, schema: Schema[A]) : Unit = {
    var components = openApi.getComponents
    if (components == null) {
      components = new Components()
      openApi.setComponents(components)
    }
    var schemas = components.getSchemas
    if (schemas == null) {
      schemas = new java.util.HashMap[String,Schema[_]]()
      components.setSchemas(schemas)
    }
    if (! components.getSchemas.containsKey(entityName)) {
      components.addSchemas(entityName, schema)
    }
  }

  private def upcertPath(openAPI: OpenAPI, path: String, f: PathItem => Unit) = {
    var paths = openAPI.getPaths
    if (paths == null) {
      paths = new Paths()
      openAPI.setPaths(paths)
    }
    var pathItem = paths.get(path)
    if (pathItem == null) {
      pathItem = new PathItem()
      paths.addPathItem(path, pathItem)
    }
    f(pathItem)

  }


  def get[A](outputSchema: (DataClass[A], String), urlPath: String): OpenAPI => OpenAPI = { openAPI =>

    val outputEntityName = outputSchema._2

    val inputSchema = SwaggerCoreInterpreter(outputSchema._1).name(outputEntityName)

    upsertComponent(openAPI, outputEntityName, inputSchema)

    val apiResponse = new ApiResponse()
      .$ref(s"#/components/schemas/${outputEntityName}")
    val apiResponses = new ApiResponses()
      .addApiResponse("200", apiResponse)

    val paramSchema = new IntegerSchema()
    val param = new Parameter()
      .name("id").in("path").required(true)
      .description(s"id of the ${outputEntityName} to retrieve")
      .schema(paramSchema)

    val operation = new Operation()
      .responses(apiResponses)
      .parameters(java.util.Collections.singletonList(param))
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Find ${outputEntityName} by ID")
      .description(s"Returns ${outputEntityName} by id")
      .operationId(s"get${outputEntityName}ById")

    upcertPath(openAPI, "/{id}", _.get(operation))

    openAPI
  }

  def delete[O](
                          outputSchemaWithName: (DataClass[O], String),
                          urlPath: String,
                          contentTypes: List[String]
                        ): OpenAPI => OpenAPI = { openAPI =>

    val outputEntityName = outputSchemaWithName._2
    val outputComponentSchema = SwaggerCoreInterpreter(outputSchemaWithName._1).name(outputEntityName)

    upsertComponent(openAPI, outputEntityName, outputComponentSchema)

    val apiResponse = new ApiResponse()
      .$ref(s"#/components/schemas/${outputEntityName}")
    val apiResponses = new ApiResponses()
      .addApiResponse("200", apiResponse)

    val paramSchema = new IntegerSchema()
    val param = new Parameter()
      .name("id").in("path").required(true)
      .description(s"id of the ${outputEntityName} to delete")
      .schema(paramSchema)

    val operation = new Operation()
      .responses(apiResponses)
      .parameters(java.util.Collections.singletonList(param))
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Delete ${outputEntityName} by ID")
      .description(s"Delete ${outputEntityName} by id")
      .operationId(s"get${outputEntityName}ById")

    upcertPath(openAPI, "/{id}", _.delete(operation))
    openAPI
  }

  def put[I,O, E](
    inputSchemaAndName: (DataClass[I], String),
    outputSchemaAndName: (DataClass[O], String),
    errorSchemaAndName: (DataClass[E], String),
    urlPath: String,
    contentTypes: List[String]
  ): OpenAPI => OpenAPI = { openAPI =>

    val inputEntityName = inputSchemaAndName._2
    val outputEntityName = outputSchemaAndName._2
    val errorEntityName = errorSchemaAndName._2

    val inputComponentSchema = SwaggerCoreInterpreter(inputSchemaAndName._1).name(inputEntityName)
    val outputComponentSchema = SwaggerCoreInterpreter(outputSchemaAndName._1).name(outputEntityName)
    val errorComponentSchema = SwaggerCoreInterpreter(errorSchemaAndName._1).name(errorEntityName)

    upsertComponent(openAPI, inputEntityName, inputComponentSchema)
    upsertComponent(openAPI, outputEntityName, outputComponentSchema)
    upsertComponent(openAPI, errorEntityName, errorComponentSchema)

    val outputComponentRef = s"#/components/schemas/${outputEntityName}"
    val inputComponentRef = s"#/components/schemas/${inputEntityName}"
    val errorComponentRef = s"#/components/schemas/${errorEntityName}"

    val apiResponses = new ApiResponses()
      .addApiResponse("200", new ApiResponse().$ref(outputComponentRef))
      .addApiResponse("400", new ApiResponse().$ref(errorComponentRef))

    val inputOasSchema = new Schema()
      .$ref(inputComponentRef)


    val operation = new Operation()
      .responses(apiResponses)
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Update ${inputEntityName}, returning ${outputEntityName}")
      .description(s"Update and return the updated ${outputEntityName}")
      .operationId(s"put${inputEntityName}")

    contentTypes.foreach(contentType => {
      val encoding = new Encoding().contentType(contentType)
      val encodings = Map((contentType, encoding))
      val mediaType = new MediaType()
        .schema(inputOasSchema)
        .encoding(encodings.asJava)

      val content = new Content()
        .addMediaType(contentType, mediaType)

      val requestBody = new RequestBody()
        .$ref(inputComponentRef)
        .content(content)

      operation.setRequestBody(requestBody)
    })


    upcertPath(openAPI, "/{id}", _.put(operation))

    openAPI

  }

  def post[I,O, E](
    inputSchemaAndName: (DataClass[I], String),
    outputSchemaAndName: (DataClass[O], String),
    errorSchemaAndName: (DataClass[E], String),
    urlPath: String,
    contentTypes: List[String]): OpenAPI => OpenAPI = { openAPI =>

    val inputEntityName = inputSchemaAndName._2
    val outputEntityName = outputSchemaAndName._2
    val errorEntityName = errorSchemaAndName._2

    val inputComponentSchema = SwaggerCoreInterpreter(inputSchemaAndName._1).name(inputEntityName)
    val outputComponentSchema = SwaggerCoreInterpreter(outputSchemaAndName._1).name(outputEntityName)
    val errorComponentSchema = SwaggerCoreInterpreter(errorSchemaAndName._1).name(errorEntityName)

    upsertComponent(openAPI, inputEntityName, inputComponentSchema)
    upsertComponent(openAPI, outputEntityName, outputComponentSchema)
    upsertComponent(openAPI, errorEntityName, errorComponentSchema)

    val outputComponentRef = s"#/components/schemas/${outputEntityName}"
    val inputComponentRef = s"#/components/schemas/${inputEntityName}"
    val errorComponentRef = s"#/components/schemas/${errorEntityName}"

    val apiResponses = new ApiResponses()
      .addApiResponse("200", new ApiResponse().$ref(outputComponentRef))
      .addApiResponse("400", new ApiResponse().$ref(errorComponentRef))

    val inputOasSchema = new Schema()
        .$ref(inputComponentRef)


    val operation = new Operation()
      .responses(apiResponses)
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Create ${inputEntityName}, returning ${outputEntityName}")
      .description(s"Create and return the newly created ${outputEntityName}")
      .operationId(s"post${inputEntityName}")

    contentTypes.foreach(contentType => {
      val encoding = new Encoding().contentType(contentType)
      val encodings = Map((contentType, encoding))
      val mediaType = new MediaType()
        .schema(inputOasSchema)
        .encoding(encodings.asJava)

      val content = new Content()
        .addMediaType(contentType, mediaType)

      val requestBody = new RequestBody()
        .$ref(inputComponentRef)
        .content(content)

      operation.setRequestBody(requestBody)
    })



    upcertPath(openAPI, "", _.post(operation))

    openAPI

  }

}
