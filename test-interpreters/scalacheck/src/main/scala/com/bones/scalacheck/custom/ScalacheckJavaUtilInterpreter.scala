package com.bones.scalacheck.custom

import java.util.UUID

import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.scalacheck.GenAlg
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait ScalacheckJavaUtilInterpreter extends GenAlg[JavaUtilValue] {
  override def gen[A](ag: JavaUtilValue[A]): Gen[A] =
    ag match {
      case _: UuidData => arbitrary[UUID]
    }
}
