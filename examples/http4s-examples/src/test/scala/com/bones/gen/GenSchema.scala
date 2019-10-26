package com.bones.gen

import com.bones.data.KvpHListValue
import com.bones.oas3.SwaggerCoreInterpreter
import com.bones.scalacheck.GenGadt
import io.swagger.v3.oas.models.media.{ObjectSchema, Schema}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAllNoShrink
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.Checkers
import shapeless.{HList, Nat}

class GenSchema extends FunSuite with Checkers {

  implicit val hListValueGen: Gen[KvpHListValue[_<:HList,_<:Nat]] = GenGadt.genHListValue()

  implicit val schema: Gen[Schema[Object]] = hListValueGen.map(hListValue => {
    SwaggerCoreInterpreter.isoInterpreter.fromValueDef(hListValue).apply( (new ObjectSchema())).asInstanceOf[Schema[Object]]
  })
  implicit val arbitrary: Arbitrary[Schema[Object]] = Arbitrary(schema)

  test("Schema Is Valid") {
    check( (swaggerSchema: Schema[Object]) => {
      val str = io.swagger.v3.core.util.Json.mapper().writeValueAsString(swaggerSchema)
//      println(str)
      true
    } )
  }

}
