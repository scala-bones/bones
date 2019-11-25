package com.bones.oas3

import com.bones.data.HListConvert
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.AllSupported
import org.scalatest.FunSuite
import com.bones.syntax._
import io.swagger.v3.oas.models.OpenAPI

class CrudOasInterpreterTest extends FunSuite {

  val idDefinition = kvp("id", int)
  val allSupportedWithId = Schemas.allSupportCaseClass match {
    case h: HListConvert[_,_,AllSupported] => {
      implicit val manifest = h.manifestOfA
      (idDefinition :: h :><: com.bones.syntax.kvpNil).tupled[(Int,AllSupported)]
    }
  }

  case class Error(message: String)
  val error = (kvp("message", string) :: kvpNil).convert[Error]


  test("all supported") {
    val openApi = CrudOasInterpreter.jsonApiForService(
      "allSupported",
      "allSupported",
      "2.0",
      List("application/json"),
      Schemas.allSupportCaseClass,
      allSupportedWithId,
      error,
      true, true, true, true, true
    )(new OpenAPI())

    val string = io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)
    println(string)
  }

}
