package com.bones.jdbc

import com.bones.jdbc.column.DbColumnInterpreter
import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class DbColumnInterpreterTest extends AnyFunSuite {

  val result = DbColumnInterpreter.tableDefinitionCustomAlgebra(Schemas.creditCardSchema, column.defaultDbColumnInterpreter)

}
