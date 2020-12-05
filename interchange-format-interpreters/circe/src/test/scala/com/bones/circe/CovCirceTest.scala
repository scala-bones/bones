package com.bones.circe

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Locale

import com.bones.circe.values._
import com.bones.interpreter.encoder.{
  Encoder,
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder
}
import com.bones.schemas.CustomCovSchema._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import shapeless.{Inl, Inr}

class CovCirceTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  test("scalacheck allSupport types - marshall then unmarshall with custom algebra") {

    val dateFormatter: DateTimeFormatter =
      DateTimeFormatter
        .ofPattern("uuuuMMddHHmmss.SX")
        .withLocale(Locale.FRANCE)
        .withZone(ZoneId.of("UTC"))

    def customAlgebraEncoder[A](algebra: CustomAlgebra[A]): Encoder[CustomAlgebra, A, Json] =
      new Encoder[CustomAlgebra, A, Json] {
        val f = algebra match {
          case MarkdownData =>
            (str: String) =>
              Json.fromString(str)
        }
        override def encode(a: A): Json = f(a.asInstanceOf[String])
      }

    def dateExtAlgebraEncoder[A](alg: DateExtAlgebra[A]): Encoder[DateExtAlgebra, A, Json] = {
      val encoder = alg match {
        case InstantData =>
          new Encoder[DateExtAlgebra, Instant, Json] {
            override def encode(i: Instant): Json =
              Json.fromString(dateFormatter.format(i))
          }
      }
      encoder.asInstanceOf[Encoder[DateExtAlgebra, A, Json]]
    }

    object BlogEncoder extends InterchangeFormatEncoderValue[BlogAlgebra, Json] {

      override def createEncoder[A](alg: BlogAlgebra[A]): Encoder[BlogAlgebra, A, Json] =
        alg match {
          case Inl(customAlgebra)       => customAlgebraEncoder(customAlgebra).encode(_)
          case Inr(Inl(dateExtAlgebra)) => dateExtAlgebraEncoder(dateExtAlgebra).encode(_)
          case Inr(Inr(Inl(scalaCoreValue))) =>
            BaseScalaCoreEncoder.createEncoder(scalaCoreValue).encode(_)
          case Inr(Inr(Inr(_))) => sys.error("Unreachable code")
        }
    }

    val encoder = new CirceEncoderInterpreter[BlogAlgebra] {
      override val coproductTypeKey: String = "type"
      override val encoder: InterchangeFormatEncoderValue[BlogAlgebra, Json] = BlogEncoder
      override val interchangeFormatEncoder: InterchangeFormatPrimitiveEncoder[Json] =
        CircePrimitiveEncoder
    }

    val blogPostToJson = encoder
      .generateEncoder(BlogPost.blogPostSchema)

    val instant = Instant.parse("2020-12-03T10:15:30.00Z")

    val blogPost = BlogPost(1, "title", List("tag1", "tag2"), instant, "Here is some content")

    val json = blogPostToJson.encode(blogPost)
    val jsonString = json.spaces2

    val expectedResult =
      """{
          |  "id" : 1,
          |  "title" : "title",
          |  "tags" : [
          |    "tag1",
          |    "tag2"
          |  ],
          |  "publishDate" : "20201203101530.0Z",
          |  "content" : "Here is some content"
          |}""".stripMargin

    jsonString mustEqual expectedResult

  }

}
