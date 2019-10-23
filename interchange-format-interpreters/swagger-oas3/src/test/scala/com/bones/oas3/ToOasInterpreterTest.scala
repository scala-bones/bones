package com.bones.oas3

import com.bones.schemas.Schemas.allSupportCaseClass
import org.scalatest.FunSuite

class ToOasInterpreterTest extends FunSuite {

  val interpreter = SwaggerCoreInterpreter.isoInterpreter

  val swaggerSchema = interpreter(allSupportCaseClass)

  val str = io.swagger.v3.core.util.Json.mapper().writeValueAsString(swaggerSchema)
//  println(str)


}
