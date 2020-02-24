package com.bones

import java.time.format.DateTimeFormatter

import com.bones.data.BonesSchema
import com.bones.swagger.SwaggerCoreInterpreter.{CustomSwaggerInterpreter, Name, noAlgebraInterpreter}
import com.bones.syntax.NoAlgebra
import io.swagger.v3.oas.models.media.Schema

package object swagger {


  /**
    * An implementation of the SwaggerCoreInterpreter using
    * ISO date as the default date interpreter.
    */
  object SwaggerIsoInterpreter extends SwaggerCoreInterpreter {
    override def localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override def localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
  }

  def fromSchemaWithAlg[ALG[_], A](gd: BonesSchema[ALG, A],
                                   customAlgebraInterpreter: CustomSwaggerInterpreter[ALG]): Name => List[(Name, Schema[_])] =
    SwaggerIsoInterpreter.fromSchemaWithAlg(gd, customAlgebraInterpreter)

  def fromSchema[ALG[_], A](gd: BonesSchema[NoAlgebra, A]): Name => List[(Name, Schema[_])] =
    fromSchemaWithAlg[NoAlgebra, A](gd, noAlgebraInterpreter)

}
