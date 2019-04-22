package com.bones.jdbc

import com.bones.schemas.Schemas
import org.scalatest.FunSuite

class DbColumnInterpreterTest extends FunSuite {

  val result = DbColumnInterpreter.tableDefinition(Schemas.creditCardSchema)

  println(result)

}
