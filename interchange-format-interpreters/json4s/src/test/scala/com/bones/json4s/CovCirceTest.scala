package com.bones.json4s

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Locale

import com.bones.interpreter.{InterchangeFormatEncoderValue, InterchangeFormatPrimitiveEncoder}
import com.bones.json4s.impl.Json4sPrimitiveEncoder
import com.bones.json4s.values.BaseScalaCoreEncoder
import com.bones.schemas.CustomCovSchema._
import org.json4s.JsonAST.JValue
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import shapeless.{Inl, Inr}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.JsonDSL._

class CovCirceTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  test("scalacheck allSupport types - marshall then unmarshall with custom algebra") {

    val dateFormatter: DateTimeFormatter =
      DateTimeFormatter
        .ofPattern("uuuuMMddHHmmss.SX")
        .withLocale(Locale.FRANCE)
        .withZone(ZoneId.of("UTC"))

    def customAlgebraEncoder[A]: CustomAlgebra[A] => A => JValue =
      alg =>
        alg match {
          case MarkdownData =>
            str =>
              JString(str.asInstanceOf[String])
      }

    def dateExtAlgebraEncoder[A]: DateExtAlgebra[A] => A => JValue =
      alg =>
        alg match {
          case InstantData =>
            i =>
              JString(dateFormatter.format(i.asInstanceOf[Instant]))
      }

    object BlogEncoder extends InterchangeFormatEncoderValue[BlogAlgebra, JValue] {

      def encode[A](alg: BlogAlgebra[A]): A => JValue =
        alg match {
          case Inl(customAlgebra)            => customAlgebraEncoder(customAlgebra)
          case Inr(Inl(dateExtAlgebra))      => dateExtAlgebraEncoder(dateExtAlgebra)
          case Inr(Inr(Inl(scalaCoreValue))) => BaseScalaCoreEncoder.encode(scalaCoreValue)
          case Inr(Inr(Inr(_)))              => sys.error("Unreachable code")
        }
    }

    val encoder = new Json4sEncoderInterpreter[BlogAlgebra] {
      override val coproductTypeKey: String = "type"
      override val encoder: InterchangeFormatEncoderValue[BlogAlgebra, JValue] = BlogEncoder
      override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[JValue] =
        Json4sPrimitiveEncoder
    }

    val blogPostToJson = encoder
      .generateEncoder(BlogPost.blogPostSchema)

    val instant = Instant.parse("2020-12-03T10:15:30.00Z")

    val blogPost = BlogPost(1, "title", List("tag1", "tag2"), instant, "Here is some content")

    val json = blogPostToJson.apply(blogPost)
    val jsonString = pretty(render(json))

    val expectedResult =
      """{
        |  "id":1,
        |  "title":"title",
        |  "tags":["tag1","tag2"],
        |  "publishDate":"20201203101530.0Z",
        |  "content":"Here is some content"
        |}""".stripMargin

    jsonString mustEqual expectedResult

  }

}
