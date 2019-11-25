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

  implicit val schema: Gen[List[Schema[Object]]] = hListValueGen.map(hListValue => {
    val all = SwaggerCoreInterpreter.isoInterpreter.fromValueDef(hListValue)("hList")
    ("hList", all.mainSchema) :: all.referenceSchemas
  }).map(_.asInstanceOf[List[Schema[Object]]]) // TODO get rid  of asInstanceOf
  implicit val arbitrary: Arbitrary[List[Schema[Object]]] = Arbitrary(schema)

  test("Schema Is Valid") {
    check( (swaggerSchema: List[Schema[Object]]) => {
      val str = io.swagger.v3.core.util.Json.mapper().writeValueAsString(swaggerSchema)
//      println(str)
      true
    } )
  }

}
