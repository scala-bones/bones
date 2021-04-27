package com.bones.akkahttp.server.search

import akka.http.scaladsl.common
import akka.http.scaladsl.common.NameReceptacle
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.RouteResult.Complete
import akka.http.scaladsl.server.directives.ParameterDirectives.ParamSpec
import akka.http.scaladsl.server.{Directive, Directive1, Route}
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, Unmarshaller}
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
import scala.concurrent.{ExecutionContext, Future}
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

//  val schema = {
//    ("color", enumeration(Color)) ::
//      ("backGroundColor", enumeration(Color)) ::
//      kvpNil
//  }

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
      (a, b, c, d, e) =>
        ???
    }

    val t1 = parameter("color")
    val t2 = parameter("backgroundColor".as[Int])

    val x2 = t1.flatMap(s1 => {
      t2.map(s2 => {
        s1 :: s2 :: HNil
      })
    })

  }

  trait ExtractParam[ALG[_], A] {

    val fromStringUnmarshaller: FromStringUnmarshallerInterpreter[ALG]
    val ec: ExecutionContext

    def fromKvpCollection[A](kvpCollection: KvpCollection[String, ALG, A]): Directive[Tuple1[A]] = {

      kvpCollection match {
        case kvp: KvpWrappedHList[String, ALG, a, h, n] @unchecked  => ???
        case kvp: KvpWrappedCoproduct[String, ALG, a, c] @unchecked => ???
        case kvp: KvpSingleValueHead[String, ALG, A, t, tl, ht] @unchecked =>
          ???
        case kvp: KvpHListCollectionHead[String, ALG, ho, no, h, hl, t, tl] @unchecked =>
          ???
        case kvp: KvpNil[String, ALG] =>
          Directive { inner => ctx =>
            inner(Tuple1(HNil.asInstanceOf[A]))(ctx)
          }
        case kvp: KvpCoproduct[String, ALG, c] => ???
      }
    }

    def kvpSingleValueHead[A, T <: HList, TL <: Nat, O <: A :: T](
      kvp: KvpSingleValueHead[String, ALG, A, T, TL, O]): Directive[Tuple1[O]] = {
      val tailDirective: Directive[Tuple1[T]] = fromKvpCollection(kvp.tail)
      val headDirective: Directive[Tuple1[A]] = kvp.head match {
        case Left(keyDefinition) => {
          keyDefinition.dataDefinition match {
            case Left(hov) => ???
            case Right(alg) =>
              ParamSpec
                .forNR[A](new common.NameReceptacle(keyDefinition.key))(
                  fromStringUnmarshaller.getFromStringUnmarshaller[A](alg))
                .get
          }
        }
        case Right(kvpCollection) => ???
      }

      for {
        hd <- headDirective
        td <- tailDirective
      } yield kvp.isHCons.cons(hd, td)

    }
  }

}
