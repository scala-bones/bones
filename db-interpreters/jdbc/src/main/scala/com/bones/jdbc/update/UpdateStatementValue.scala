package com.bones.jdbc.update

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object UpdateStatementValue {

  /** using kind projector allows us to create a new interpreter by merging two existing
    * interpreters. see https://stackoverflow.com/a/60561575/387094
    */
  def merge[L[_], R[_] <: Coproduct](
    li: UpdateStatementValue[L],
    ri: UpdateStatementValue[R]
  ): UpdateStatementValue[Lambda[A => L[A] :+: R[A]]] =
    new UpdateStatementValue[Lambda[A => L[A] :+: R[A]]] {

      override def definitionResult[A](alg: L[A] :+: R[A]): (Index, Key) => JdbcColumnStatement[A] =
        alg match {
          case Inl(l) => li.definitionResult(l)
          case Inr(r) => ri.definitionResult(r)
        }

    }

  implicit class InterpreterOps[ALG[_]](val base: UpdateStatementValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: UpdateStatementValue[R]
    ): UpdateStatementValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }

  object CNilUpdateStatementInterpreter$ extends UpdateStatementValue[CNilF] {
    override def definitionResult[A](alg: CNilF[A]): (Index, Key) => JdbcColumnStatement[A] =
      sys.error("Unreachable code")
  }
}

trait UpdateStatementValue[ALG[_]] {
  def definitionResult[A](alg: ALG[A]): (Index, Key) => JdbcColumnStatement[A]
}
