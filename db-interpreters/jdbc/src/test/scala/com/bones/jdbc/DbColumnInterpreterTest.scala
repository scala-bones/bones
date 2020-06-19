package com.bones.jdbc

import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class DbColumnInterpreterTest extends AnyFunSuite {

  val result = DbColumnInterpreter.tableDefinition(Schemas.creditCardSchema)


}
