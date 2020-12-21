package com.bones.interpreter.encoder

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object InterchangeFormatEncoderValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, OUT](
    li: InterchangeFormatEncoderValue[L, OUT],
    ri: InterchangeFormatEncoderValue[R, OUT]
  ): InterchangeFormatEncoderValue[Lambda[V => L[V] :+: R[V]], OUT] =
    new InterchangeFormatEncoderValue[Lambda[V => L[V] :+: R[V]], OUT] {
      override def generateEncoder[A](
        alg: L[A] :+: R[A]): Encoder[Lambda[V => L[V] :+: R[V]], A, OUT] = {
        new Encoder[Lambda[A => L[A] :+: R[A]], A, OUT] {
          val encoder = alg match {
            case Inl(l) => li.generateEncoder(l).encode(_)
            case Inr(r) => ri.generateEncoder(r).encode(_)
          }
          override def encode(a: A): OUT = encoder(a)
        }
      }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: InterchangeFormatEncoderValue[ALG, OUT])
      extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: InterchangeFormatEncoderValue[R, OUT]
    ): InterchangeFormatEncoderValue[Lambda[A => ALG[A] :+: R[A]], OUT] =
      merge(base, r)

  }

  case class CNilInterchangeFormatEncoder[OUT]() extends InterchangeFormatEncoderValue[CNilF, OUT] {
    override def generateEncoder[A](alg: CNilF[A]): Encoder[CNilF, A, OUT] =
      new Encoder[CNilF, A, OUT] {
        override def encode(a: A): OUT = sys.error("Unreachable code")
      }
  }

}

trait InterchangeFormatEncoderValue[ALG[_], OUT] {
  def generateEncoder[A](alg: ALG[A]): Encoder[ALG, A, OUT]
}
