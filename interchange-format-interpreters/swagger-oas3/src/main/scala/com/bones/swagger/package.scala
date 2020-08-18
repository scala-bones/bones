package com.bones

import com.bones.data.PrimitiveWrapperValue
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name}
import io.swagger.v3.oas.models.media.Schema

package object swagger {

  /**
    * An implementation of the SwaggerCoreInterpreter using
    * ISO date as the default date interpreter.
    */
  object SwaggerIsoInterpreter extends SwaggerCoreInterpreter

  def fromSchemaWithAlg[ALG[_], A](
    gd: PrimitiveWrapperValue[ALG, A],
    customAlgebraInterpreter: CustomSwaggerInterpreter[ALG]): Name => List[(Name, Schema[_])] =
    SwaggerIsoInterpreter.generateSchemas(gd, customAlgebraInterpreter)

}
