package com.bones.tapir

import com.bones.schemas.Schemas.allSupportedSchema
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import sttp.tapir.Schema

class BonesToTapirTransformationTest extends AnyFunSuite with Matchers {

  // TODO: finsish coproduct (if possible)
  ignore("transform to tapir schema") {
    val bonesSchema = allSupportedSchema
    val result = values.defaultTransformation.kvpToSchema(bonesSchema)
    println(result.show)
  }
}
