package com.bones.interpreter

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object InterchangeFormatEncoder {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
    li: InterchangeFormatEncoder[L, OUT],
    ri: InterchangeFormatEncoder[R, OUT]
  ): InterchangeFormatEncoder[Lambda[A => L[A] :+: R[A]], OUT] =
    new InterchangeFormatEncoder[Lambda[A => L[A] :+: R[A]], OUT] {
      override def encode[A](lr: L[A] :+: R[A]): A => OUT = lr match {
        case Inl(l) => li.encode(l)
        case Inr(r) => ri.encode(r)
      }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: InterchangeFormatEncoder[ALG, OUT])
      extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: InterchangeFormatEncoder[R, OUT]
    ): InterchangeFormatEncoder[Lambda[A => ALG[A] :+: R[A]], OUT] =
      merge(base, r)

  }

  case class CNilInterchangeFormatEncoder[OUT]() extends InterchangeFormatEncoder[CNilF, OUT] {
    override def encode[A](alg: CNilF[A]): A => OUT = sys.error("Unreachable code")
  }

}

trait InterchangeFormatEncoder[ALG[_], OUT] {
  def encode[A](alg: ALG[A]): A => OUT
}
