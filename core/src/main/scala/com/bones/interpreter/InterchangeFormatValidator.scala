package com.bones.interpreter

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object InterchangeFormatValidator {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
    li: InterchangeFormatValidator[L, OUT],
    ri: InterchangeFormatValidator[R, OUT]
  ): InterchangeFormatValidator[Lambda[A => L[A] :+: R[A]], OUT] =
    new InterchangeFormatValidator[Lambda[A => L[A] :+: R[A]], OUT] {
      override def validate[A](lr: L[A] :+: R[A])
        : (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] = lr match {
        case Inl(l) => li.validate(l)
        case Inr(r) => ri.validate(r)
      }

    }

  implicit class InterpreterOps[ALG[_], OUT](val base: InterchangeFormatValidator[ALG, OUT])
      extends AnyVal {
    def ++[R[_] <: Coproduct](r: InterchangeFormatValidator[R, OUT])
      : InterchangeFormatValidator[Lambda[A => ALG[A] :+: R[A]], OUT] =
      merge(base, r)

  }

  case class CNilInterchangeFormatValidator[OUT]() extends InterchangeFormatValidator[CNilF, OUT] {
    override def validate[A](
      alg: CNilF[A]): (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] =
      sys.error("Unreachable code")
  }
}

trait InterchangeFormatValidator[ALG[_], IN] {
  def validate[A](
    alg: ALG[A]): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A]
}
