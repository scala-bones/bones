package com.bones.json4s

import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors
import com.bones.schemas.Schemas
import org.json4s._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers
import shapeless.{HList, HNil, Poly1, UnaryTCConstraint}
import com.bones.syntax._

class AllSupportedDeltaValidationTest extends AnyFunSuite with Checkers {

  test("all supported delta") {

    val simpleSchema =
      ("long", long()) ::
        ("int", int()) ::
        kvpNil

    val alg = Schemas.allSupportedSchema
    val deltaValidator =
      com.bones.json4s.values.isoJson4sDeltaValidatorInterpreter.fromKvpHListCollection(alg)

//    deltaValidator.hListRR

//    def asInst[H] = new Poly1 {
//      implicit val intCase: Case.Aux[Int, Either[ExtractionErrors[String], Int]] =
//        at(_.asInstanceOf[Either[ExtractionErrors[String], Int]])
//      implicit val longCase: Case.Aux[Long, Either[ExtractionErrors[String], Long]] =
//        at(_.asInstanceOf[Either[ExtractionErrors[String], Long]])
//
//    }

    deltaValidator.validate(JObject(List.empty), List.empty) match {
      case Left(err) => fail(s"unexpected errors: ${err}")
      case Right(h) => {
        println(h)
      }
    }

//    val longFun = (_: Long) => "long"
//    val intFun = (_: Int) => "int"
//    val strFun = identity(_)

//    val toString = longFun :: intFun :: HNil
//    val inputs = Option(1L) :: Option(2) :: None :: HNil
//
//    case class Result[L <: HList](l: L)(
//      implicit utcc: UnaryTCConstraint[L, CanBeOmitted[String, *]])
//
//    def doIt[L <: HList, F <: HList](l: L, f: F)(
//      implicit utcc: UnaryTCConstraint[L, CanBeOmitted[String, *]],
//      fuc: UnaryTCConstraint[F, Function1[*, String]]
//    ) = {
//      (l, f) match {
//        case (HNil, HNil) => HNil
//        case (cbo, func) => {
//          val head = cbo.map(func).getOrElse("Empty")
//          head :: doIt()
//        }
//      }
//    }

    val someValues = JObject(("long", JLong(55)), ("string", JString("Hello World")))
    deltaValidator.validate(someValues, List.empty) match {
      case Left(err) => fail(s"unexpected errors: ${err}")
      case Right(h) => {
        implicit val uc = deltaValidator.hListRR
        h
      }
    }

    val invalidValues = JObject(("long", JLong(-55)), ("int", JInt(1000)))
    deltaValidator.validate(invalidValues, List.empty) match {
      case Left(err) => //println(err)
      case Right(h)  => fail("Expected errors")
    }

  }

}
