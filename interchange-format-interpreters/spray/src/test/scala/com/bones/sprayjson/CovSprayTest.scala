package com.bones.sprayjson

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Locale

import com.bones.interpreter.encoder.{
  Encoder,
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder
}
import com.bones.schemas.CustomCovSchema._
import com.bones.sprayjson.impl.SprayPrimitiveEncoder
import com.bones.sprayjson.values.BaseScalaCoreEncoder
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import shapeless.{Inl, Inr}
import spray.json.DefaultJsonProtocol._
import spray.json.{JsValue, _}

class CovSprayTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  test("scalacheck allSupport types - marshall then unmarshall with custom algebra") {

    val dateFormatter: DateTimeFormatter =
      DateTimeFormatter
        .ofPattern("uuuuMMddHHmmss.SX")
        .withLocale(Locale.FRANCE)
        .withZone(ZoneId.of("UTC"))

    def customAlgebraEncoder[A](alg: CustomAlgebra[A]): Encoder[CustomAlgebra, A, JsValue] =
      alg match {
        case MarkdownData =>
          str => str.asInstanceOf[String].toJson
      }

    def dateExtAlgebraEncoder[A](alg: DateExtAlgebra[A]): Encoder[DateExtAlgebra, A, JsValue] =
      alg match {
        case InstantData =>
          i => dateFormatter.format(i.asInstanceOf[Instant]).toJson
      }

    object BlogEncoder extends InterchangeFormatEncoderValue[BlogAlgebra, JsValue] {

      override def generateEncoder[A](alg: BlogAlgebra[A]): Encoder[BlogAlgebra, A, JsValue] =
        alg match {
          case Inl(customAlgebra)       => customAlgebraEncoder(customAlgebra).encode(_)
          case Inr(Inl(dateExtAlgebra)) => dateExtAlgebraEncoder(dateExtAlgebra).encode(_)
          case Inr(Inr(Inl(scalaCoreValue))) =>
            BaseScalaCoreEncoder.generateEncoder(scalaCoreValue).encode(_)
          case Inr(Inr(Inr(_))) => sys.error("Unreachable code")
        }
    }

    val encoder = new SprayEncoderInterpreter[BlogAlgebra] {
      override val coproductTypeKey: String = "type"
      override val encoder: InterchangeFormatEncoderValue[BlogAlgebra, JsValue] = BlogEncoder
      override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[JsValue] =
        SprayPrimitiveEncoder
    }

    val blogPostToJson = encoder
      .generateEncoder(BlogPost.blogPostSchema)

    val instant = Instant.parse("2020-12-03T10:15:30.00Z")

    val blogPost = BlogPost(1, "title", List("tag1", "tag2"), instant, "Here is some content")

    val json = blogPostToJson.encode(blogPost)
    val jsonString = json.prettyPrint

    val expectedResult =
      """{
          |  "content": "Here is some content",
          |  "id": 1,
          |  "publishDate": "20201203101530.0Z",
          |  "tags": ["tag1", "tag2"],
          |  "title": "title"
          |}""".stripMargin

    jsonString mustEqual expectedResult

  }

}
