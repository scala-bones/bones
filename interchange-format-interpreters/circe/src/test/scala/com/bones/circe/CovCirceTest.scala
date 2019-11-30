package com.bones.circe

import java.nio.charset.Charset

import com.bones.scalacheck.Scalacheck
import com.bones.schemas.CovSchemas.{AllSupported, allSupportCaseClass}
import org.scalacheck.Arbitrary
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers
import com.bones.schemas.CovSchemas._
import io.circe.Json


class CovCirceTest extends FunSuite with Checkers with MustMatchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  test("scalacheck allSupport types - marshall then marshall") {
    val validateFromCirce = CovCirceEncoderInterpreter.isoInterpreterCov

    //  val jsonToCc = validateFromCirce.byteArrayFuncFromSchema(allSupportCaseClass, Charset.forName("UTF8"))

    def customAlgebraEncoder[A]: CustomAlgebra[A] => A => Json = alg => alg match {
      case MarkdownData => str => Json.fromString(str)
    }

    val 
    def dateExtAlgebraEncoder[A]: DateExtAlgebra[A] => A => Json = alg => alg match {
      case InstantData => i => Json.fromString()
    }


    val ccToJson = CovCirceEncoderInterpreter.isoInterpreterCov().fromSchema(BlogPost.baseSchema)

//    implicit val arb = Arbitrary(Scalacheck.valueDefinition(allSupportCaseClass))
//    val utf8 = Charset.forName("UTF8")


    val json = ccToJson.apply(allSupportedInstance)
    val jsonString = json.spaces2.getBytes()

    println(jsonString)

//    val newCc = jsonToCc(jsonString)
//    newCc match {
//      case Left(x) =>
//        fail(s"expected success, received $x for JSON string ${io.circe.parser.parse(new String(jsonString, utf8))}")
//      case Right(newCc2) =>
//        newCc2.fancyEquals(cc)
//    }

  }

}
