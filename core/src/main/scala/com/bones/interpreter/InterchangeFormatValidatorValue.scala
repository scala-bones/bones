package com.bones.interpreter

import com.bones.data.Error.ExtractionError
import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object InterchangeFormatValidatorValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters */
  def merge[L[_], R[_] <: Coproduct, IN](
    li: InterchangeFormatValidatorValue[L, IN],
    ri: InterchangeFormatValidatorValue[R, IN]
  ): InterchangeFormatValidatorValue[Lambda[A => L[A] :+: R[A]], IN] =
    new InterchangeFormatValidatorValue[Lambda[A => L[A] :+: R[A]], IN] {
      override def createValidator[AA](
        lr: L[AA] :+: R[AA]): OptionalInputValidator[String, Lambda[A => L[A] :+: R[A]], AA, IN] =
        lr match {
          case Inl(l) => li.createValidator(l).validateWithPath(_, _)
          case Inr(r) => ri.createValidator(r).validateWithPath(_, _)
        }
    }

  implicit class InterpreterOps[ALG[_], IN](val base: InterchangeFormatValidatorValue[ALG, IN])
      extends AnyVal {
    def ++[R[_] <: Coproduct](r: InterchangeFormatValidatorValue[R, IN])
      : InterchangeFormatValidatorValue[Lambda[A => ALG[A] :+: R[A]], IN] =
      merge(base, r)

  }

  case class CNilInterchangeFormatValidator[IN]()
      extends InterchangeFormatValidatorValue[CNilF, IN] {
    override def createValidator[A](alg: CNilF[A]): OptionalInputValidator[String, CNilF, A, IN] =
      sys.error("Unreachable code")
  }
}

trait InterchangeFormatValidatorValue[ALG[_], IN] {
  def createValidator[A](alg: ALG[A]): OptionalInputValidator[String, ALG, A, IN]
}
