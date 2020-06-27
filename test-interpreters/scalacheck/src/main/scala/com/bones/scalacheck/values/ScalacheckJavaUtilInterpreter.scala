package com.bones.scalacheck.values

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.scalacheck.GenValue
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen

trait ScalacheckJavaUtilInterpreter extends GenValue[JavaUtilValue] {
  override def gen[A](ag: JavaUtilValue[A]): Gen[A] =
    ag match {
      case _: UuidData => arbitrary[UUID]
    }
}
