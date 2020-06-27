package com.bones.gen

import com.bones.data.KvpHListValue
import com.bones.data.values.DefaultValues
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import shapeless.{HList, Nat}

class GenSchema extends AnyFunSuite with Checkers {

//  implicit val hListValueGen: Gen[KvpHListValue[AllCustomAlgebras,_<:HList,_<:Nat]] = ???

  // TODO uncomment when HListValue becomes a bones schema subtype.

//   implicit val schema: Gen[List[Schema[Object]]] = hListValueGen.map(hListValue => {
//     val all = SwaggerCoreInterpreter.isoInterpreter.fromKvpHList(hListValue, SwaggerCoreInterpreter.noAlgebraInterpreter)("hList")
//     ("hList", all.mainSchema) :: all.referenceSchemas
//   }).map(_.asInstanceOf[List[Schema[Object]]]) // TODO get rid  of asInstanceOf
//   implicit val arbitrary: Arbitrary[List[Schema[Object]]] = Arbitrary(schema)

//   test("Schema Is Valid") {
//     check( (swaggerSchema: List[Schema[Object]]) => {
//       val str = io.swagger.v3.core.util.Json.mapper().writeValueAsString(swaggerSchema)
// //      println(str)
//       true
//     } )
//   }

}
