package com.bones.react

import com.bones.schemas.Schemas
import org.scalatest.FunSuite

class CreateReactFileTest extends FunSuite {


  test("createFile") {
    println(CreateReactFile.convert(Schemas.creditCardSchema))
  }

}
