package com.bones.jdbc

import com.bones.jdbc.column.{DbColumnInterpreter, defaultDbColumnInterpreter}
import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class DbColumnInterpreterTest extends AnyFunSuite {

  val result = defaultDbColumnInterpreter.tableDefinitionCustomAlgebra(Schemas.creditCardSchema)

}
