package com.bones.akkahttp.server.search

import akka.http.scaladsl.common.NameReceptacle
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.directives.ParameterDirectives.ParamSpec
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import akka.http.scaladsl.unmarshalling.Unmarshaller
import com.bones.data.Error.{CanNotConvert, ExtractionError, ValidationError}
import com.bones.data.template.{KvpCollectionFunctor, KvpCollectionMatch}
import com.bones.data.values.{JavaTimeValue, ScalaCoreValue}
import com.bones.data.{
  KvpCollection,
  KvpCoproduct,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList
}
import com.bones.syntax._
import shapeless.{Coproduct, HList, Nat}
import shapeless._
import shapeless.ops.hlist.{Align, Mapped}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

class ParameterSearch {

  object Color extends Enumeration {
    val USD = Value("red")
    val CAD = Value("blue")
    val GBP = Value("green")
  }

  case class SearchSortPage[A](
    a: A,
    sortBy: Option[String],
    limit: Option[String],
    offset: Option[String])

  val schema = {
    ("color", enumeration(Color)) ::
      ("backGroundColor", enumeration(Color)) ::
      kvpNil
  }

  def endpointa(): Route = {
    get {
      parameter("one", "two") { (one, two) =>
        complete("done")
      }
    }
  }

  def endpoint(): Route = {
    get {
      pathSingleSlash {
        val ab = parameters("color").flatMap(a => parameter("bgColor".as[Int]).map(b => (a, b)))
        complete {
          s"Captain on the bridge! ${ab.ev}"
        }
      }
    }
  }

  def search = {
    val x = parameters("color".as[Int], "backgroundColor".?, "sortBy", "limit", "offset") {
      (color, bgColor) =>
      }
    val t1 = parameter("color")
    val t2 = parameter("backgroundColor".as[Int])

    val x = t1.flatMap(s1 => {
      t2.map(s2 => {
        s1 :: s2 :: HNil
      })
    })

    x
  }

  class ExtractParam[IN, ALG[_], A] {

    def fromKvpCollection[A](kvpCollection: KvpCollection[String, ALG, A])(
      implicit mapped: Mapped.Aux[IN, Option, A]): IN = {
      mapped
      kvpCollection match {
        case kvp: KvpWrappedHList[K, ALG, a, h, n] @unchecked  => kvpWrappedHList(kvp)
        case kvp: KvpWrappedCoproduct[K, ALG, a, c] @unchecked => kvpWrappedCoproduct(kvp)
        case kvp: KvpSingleValueHead[K, ALG, A, t, tl, ht] @unchecked =>
          kvpSingleValueHead[A, t, tl, ht](kvp)
        case kvp: KvpHListCollectionHead[K, ALG, ho, no, h, hl, t, tl] @unchecked =>
          kvpHListCollectionHead(kvp)
        case kvp: KvpNil[K, ALG]          => kvpNil(kvp)
        case kvp: KvpCoproduct[K, ALG, c] => kvpCoproduct(kvp)
      }
    }
  }

  class ToSearch[A, ALG[_]] extends KvpCollectionMatch[String, ALG, Directive1[A]] {

    val unmarshallToHNil: Unmarshaller[String, HNil] = Unmarshaller.strict[String, HNil] { _ =>
      HNil
    }

    override def kvpNil(kvp: KvpNil[String, ALG]): Directive1[A] =
      ParamSpec
        .forNR(new NameReceptacle[HNil](""))(unmarshallToHNil)
        .get
        .asInstanceOf[Directive1[A]]

    override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
      kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]): Directive1[A] = {
      val tail = fromKvpCollection(kvp.tail)
      val head = kvp.head match {
        case Left(keyDef) => {
          val primitiveExtractor: ToPrimitiveExtractor[ALG, H] = ???
          val nameReceptacle = new NameReceptacle[primitiveExtractor.B](keyDef.key)
          ParamSpec.forNR(nameReceptacle)
        }
        case Right(kvpCollection) => fromKvpCollection(kvpCollection)
      }
    }
  }

  trait ToPrimitiveExtractor[ALG[_], A] {
    type B
    def f(b: B): Either[ExtractionError[String], A]
  }
  object StringExtractor extends ToPrimitiveExtractor[ScalaCoreValue, String] {
    override type B = String
    override def f(b: String) = Right(b)

  }
  object DateExtractor extends ToPrimitiveExtractor[JavaTimeValue, LocalDateTime] {
    val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override type B = String
    override def f(b: String) =
      Try {
        dateFormatter.parse(b)
      }.toEither.left.map(_ => CanNotConvert(List.empty, b))

  }

}
