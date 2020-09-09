package com.bones.swagger

import com.bones.schemas.Schemas.allSupportCaseClass
import com.bones.syntax._
import org.scalatest.funsuite.AnyFunSuite

class ToOasInterpreterTest extends AnyFunSuite {

  val swaggerSchema = com.bones.swagger.values.defaultSwaggerInterpreter
    .generateSchemas(allSupportCaseClass)("root")
  val str = swaggerSchema
    .map(schemas => io.swagger.v3.core.util.Yaml.mapper().writeValueAsString(schemas._2))
    .mkString("\n")

  test("simpleTypeWorks") {
    case class Simple(id: Int, name: String)
    val simpleBone = (("id", int) :: ("name", string) :: kvpNil).convert[Simple]
    val simpleSwagger = com.bones.swagger.values.defaultSwaggerInterpreter
      .generateSchemas(simpleBone)("root")
    val simpleStr = io.swagger.v3.core.util.Yaml.mapper().writeValueAsString(simpleSwagger.head._2)
    //    println(simpleStr)
  }

}
