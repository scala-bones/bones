package com.bones.shapeless

import org.scalatest.funsuite.AnyFunSuite
import shapeless.{::, HList, HNil, UnaryTCConstraint}
import shapeless.syntax.std.tuple._
class UtilSpec extends AnyFunSuite {

  test("can apply hlist to hlist") {

    val values = "Hello World" :: 5 :: "Goodbye World" :: HNil
    val funcs =
      ((_: String) => 1) ::
        ((_: Int) => 2) ::
        ((_: String) => 3) ::
        HNil

//    def applyFunction[VH, VT <: HList, FT <: HList, RT](v: VH :: VT, f: Function[VH, Int] :: FT)(
//      implicit ev: VT =:= FT,
//      utcc: UnaryTCConstraint[FT, Function[*, Int]]
//    ): RT = {
//      val result = f.head.apply(v.head)
//
//      val tailResult = (v.tail, f.tail) match {
//        case (HNil, HNil) => HNil
//        case (vHead :: vTail, hHead :: hTail) => {
//          applyFunction(vHead :: vTail, hHead.asInstanceOf[Function[vHead.type, Int]] :: hTail)
//        }
//      }
//      result :: tailResult
//    }

  }
}
