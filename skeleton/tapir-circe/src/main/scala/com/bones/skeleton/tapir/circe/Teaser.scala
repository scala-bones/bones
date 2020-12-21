package com.bones.skeleton.tapir.circe

import sttp.tapir._
import sttp.tapir.generic.auto._
import com.bones.syntax._
import com.bones.skeleton.tapir.circe.DefaultAlgebras._
import sttp.tapir.generic.auto._
//import io.circe.generic.auto._
//import sttp.tapir.json.circe.jsonBody

object Teaser {

  type Limit = Int
  type AuthToken = String
  case class BooksFromYear(genre: String, year: Int)
  case class Book(title: String)

  val booksFromYearBonesSchema = (
    ("genre", string(sv.valid("mystery", "comedy"))) ::
      ("year", int(iv.positive, iv.max(2020))) ::
      kvpNil
  ).convert[BooksFromYear]

  val bookBonesSchema = (("title", string(sv.words)) :: kvpNil).convert[Book]
  val (tapirBookSchema, tapirBookCodec) = DefaultAlgebras.createTapirSchemaAndCodec(bookBonesSchema)

  val (tapirBooksForYearSchema, tapirBooksForYearCodec) =
    DefaultAlgebras.createTapirSchemaAndCodec(bookBonesSchema)

  // Define an endpoint
  val booksListing: Endpoint[(BooksFromYear, Limit, AuthToken), String, List[Book], Nothing] =
    endpoint.get
      .in(("books" / path[String]("genre") / path[Int]("year")).mapTo(BooksFromYear))
      .in(query[Limit]("limit").description("Maximum number of books to retrieve"))
      .in(header[AuthToken]("X-Auth-Token"))
      .errorOut(stringBody)
      .out(jsonBodyList(bookBonesSchema))

}
