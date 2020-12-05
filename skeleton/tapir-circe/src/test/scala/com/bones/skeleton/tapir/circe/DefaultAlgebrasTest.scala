package com.bones.skeleton.tapir.circe

import java.time.LocalDate

import org.scalatest.funspec.AnyFunSpec
import com.bones.syntax._
import org.scalatest.funsuite.AnyFunSuite

class DefaultAlgebrasTest extends AnyFunSuite {

  case class MyTapirEntity(name: String, rating: Int, date: LocalDate)

  val myTapirEntityHList =
    ("name", string(sv.words)) ::
      ("rating", int(iv.inRange(1 to 10))) ::
      ("date", localDate(jt_ld.min(LocalDate.of(2020, 1, 1)))) ::
      kvpNil

  val myTapirEntitySchema = myTapirEntityHList.convert[MyTapirEntity]

  test("Teaser example") {}

}
