package com.bones.jdbc.ideal

import com.bones.data.values.DefaultValues
import com.bones.jdbc.longId
import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class IdealTest extends AnyFunSuite {

  test("basic test") {

    val schema = Schemas.allSupportCaseClass

    val result = defaultIdealInterpreter.toIdeal(longId, schema)

    result

  }

}
