package com.bones.circe

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Locale

import com.bones.circe.custom.BaseScalaCoreEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.schemas.CustomCovSchema._
import io.circe.Json
import org.scalatestplus.scalacheck.Checkers
import shapeless.{Inl, Inr}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class CovCirceTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  test("scalacheck allSupport types - marshall then unmarshall with custom algebra") {

    val dateFormatter: DateTimeFormatter =
      DateTimeFormatter
        .ofPattern("uuuuMMddHHmmss.SX")
        .withLocale(Locale.FRANCE)
        .withZone(ZoneId.of("UTC"))

    def customAlgebraEncoder[A]: CustomAlgebra[A] => A => Json =
      alg =>
        alg match {
          case MarkdownData =>
            str =>
              Json.fromString(str.asInstanceOf[String])
      }

    def dateExtAlgebraEncoder[A]: DateExtAlgebra[A] => A => Json =
      alg =>
        alg match {
          case InstantData =>
            i =>
              Json.fromString(dateFormatter.format(i.asInstanceOf[Instant]))
      }

    object BlogEncoder extends InterchangeFormatEncoder[BlogAlgebra, Json] {

      def encode[A](alg: BlogAlgebra[A]): A => Json =
        alg match {
          case Inl(customAlgebra)            => customAlgebraEncoder(customAlgebra)
          case Inr(Inl(dateExtAlgebra))      => dateExtAlgebraEncoder(dateExtAlgebra)
          case Inr(Inr(Inl(scalaCoreValue))) => BaseScalaCoreEncoder.encode(scalaCoreValue)
          case Inr(Inr(Inr(_)))              => sys.error("Unreachable code")
        }
    }

    val blogPostToJson = IsoCirceEncoderAndValidatorInterpreter
      .encoderFromCustomSchema(BlogPost.blogPostSchema, BlogEncoder)

    val instant = Instant.parse("2020-12-03T10:15:30.00Z")

    val blogPost = BlogPost(1, "title", List("tag1", "tag2"), instant, "Here is some content")

    val json = blogPostToJson.apply(blogPost)
    val jsonString = json.spaces2

    val expectedResult = """{
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
