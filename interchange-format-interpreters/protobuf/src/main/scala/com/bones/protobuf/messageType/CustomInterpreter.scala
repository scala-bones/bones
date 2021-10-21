package com.bones.protobuf.messageType

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object CustomInterpreter {

  /** using kind projector allows us to create a new interpreter by merging two existing
    * interpreters. see https://stackoverflow.com/a/60561575/387094
    */
  def merge[L[_], R[_] <: Coproduct](
    li: CustomInterpreter[L],
    ri: CustomInterpreter[R]
  ): CustomInterpreter[Lambda[A => L[A] :+: R[A]]] =
    new CustomInterpreter[Lambda[A => L[A] :+: R[A]]] {
      override def toMessageField[A](
        lr: L[A] :+: R[A]
      ): (Name, Index) => (MessageField, Vector[NestedType], Index) =
        lr match {
          case Inl(l) => li.toMessageField(l)
          case Inr(r) => ri.toMessageField(r)
        }
    }

  implicit class InterpreterOps[ALG[_]](val base: CustomInterpreter[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: CustomInterpreter[R]
    ): CustomInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

}

trait CustomInterpreter[ALG[_]] {
  def toMessageField[A](alg: ALG[A]): (Name, Int) => (MessageField, Vector[NestedType], Int)
}

object CNilProtoFileCustomInterpreterEncoder extends CustomInterpreter[CNilF] {
  override def toMessageField[A](
    alg: CNilF[A]
  ): (Name, Index) => (MessageField, Vector[NestedType], Index) =
    sys.error("unreachable code")
}
