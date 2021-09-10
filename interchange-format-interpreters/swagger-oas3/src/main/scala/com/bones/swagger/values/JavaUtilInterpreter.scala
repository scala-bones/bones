package com.bones.swagger.values

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.swagger.SwaggerCoreInterpreter
import com.bones.swagger.SwaggerCoreInterpreter.{
  CustomSwaggerInterpreter,
  Name,
  addUuidSchema,
  exampleUuid,
  validations
}
import io.swagger.v3.oas.models.media.Schema

trait JavaUtilInterpreter extends CustomSwaggerInterpreter[JavaUtilValue] {

  override def toSchema[A](
    vd: JavaUtilValue[A],
    description: Option[String],
    example: Option[A]
  ): Name => SwaggerCoreInterpreter.SwaggerSchemas[Schema[_]] = {

    vd match {
      case ud: UuidData =>
        name =>
          addUuidSchema(
            name,
            description.getOrElse("value of type UUID"),
            example.getOrElse(exampleUuid).asInstanceOf[UUID],
            validations(ud.validations)
          )
    }

  }
}
