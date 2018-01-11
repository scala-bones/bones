package com.gaia.soy

import cats.free.FreeApplicative
import cats.implicits._

object actions {

  trait Ops[A]
  case class StringOp() extends Ops[String]
  case class OptionalStringOp() extends Ops[String]
  case class Construct2Op[A,B,T](o1: Ops[A], o2: Ops[B], f: (A,B) => T) extends Ops[T]


  trait ApOp[A]
  case class ExtractString(key: String) extends ApOp[StringOp]
  case class OptionalString(key: String) extends ApOp[OptionalStringOp]
  case class Construct2[A,B,T](o1: Ops[A], o2: Ops[B], f: (A,B) => T) extends ApOp[Construct2Op[A,B,T]]

  type Ap[A] = FreeApplicative[ApOp, A]


  def extractString(key: String): Ap[StringOp] = FreeApplicative.lift(ExtractString(key))
  def optionalString(key: String): Ap[OptionalStringOp] = FreeApplicative.lift(OptionalString(key))
  def construct2[A,B,T](o1: Ops[A], o2: Ops[B], f: (A,B) => T): Ap[Construct2Op[A,B,T]] = FreeApplicative.lift(Construct2(o1, o2, f))

  case class Two(one: String, two: Option[String])


}
