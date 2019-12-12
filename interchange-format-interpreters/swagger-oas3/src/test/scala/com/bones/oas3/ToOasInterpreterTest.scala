package com.bones.oas3

import com.bones.schemas.Schemas.allSupportCaseClass
import org.scalatest.FunSuite
import com.bones.syntax._

class ToOasInterpreterTest extends FunSuite {

  val interpreter = SwaggerCoreInterpreter.isoInterpreter

  val swaggerSchema = interpreter.fromSchema(allSupportCaseClass)("root")
//  val str = io.swagger.v3.core.util.Json.mapper().writeValueAsString(swaggerSchema)
  val str = swaggerSchema.map(schemas => io.swagger.v3.core.util.Yaml.mapper().writeValueAsString(schemas._2)).mkString("\n")
  println(str)

  test("simpleTypeWorks") {
    case class Simple(id: Int, name: String)
    val simpleBone = (kvp("id", int) :: kvp("name", string) :: kvpNil).convert[Simple]
    val simpleSwagger = interpreter.fromSchema(simpleBone)("root")
    val simpleStr = io.swagger.v3.core.util.Yaml.mapper().writeValueAsString(simpleSwagger.head._2)
    println(simpleStr)
  }


}
