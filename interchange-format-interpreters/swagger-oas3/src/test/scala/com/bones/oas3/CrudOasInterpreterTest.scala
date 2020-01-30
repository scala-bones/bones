package com.bones.oas3

import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.AllSupported
import org.scalatest.FunSuite
import com.bones.syntax._
import io.swagger.v3.oas.models.OpenAPI

class CrudOasInterpreterTest extends FunSuite {

  val idDefinition = ("id", int)

  val allSupportedWithId = (idDefinition :<: Schemas.allSupportCaseClass :><: kvpNil).tupled[(Int, AllSupported)]

  case class Error(message: String)
  val error = (("message", string) :<: kvpNil).convert[Error]


  test("all supported") {
    val openApi = CrudOasInterpreter.jsonApiForService(
      "allSupported",
      "allSupported",
      "2.0",
      List("application/json"),
      Schemas.allSupportCaseClass,
      allSupportedWithId,
      error,
      SwaggerCoreInterpreter.noAlgebraInterpreter,
      true, true, true, true, true
    )(new OpenAPI())

    val string = io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)
//    println(string)
  }

}
