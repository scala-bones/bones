package com.bones.jdbc.ideal

import com.bones.data.values.DefaultValues
import com.bones.schemas.Schemas
import org.scalatest.funsuite.AnyFunSuite

class IdealTest extends AnyFunSuite {


  test("basic test") {

    val schema = Schemas.allSupportedSchema

    Ideal[DefaultValues].toIdeal(schema)

  }

}
