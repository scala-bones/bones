package com.bones.jdbc.update

import com.bones.data.values.CNilF
import com.bones.jdbc.update.DbUpdate.{DefinitionResult, Index, Key}
import shapeless.{:+:, Coproduct, Inl, Inr}

object DbUpdateValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
                                              li: DbUpdateValue[L],
                                              ri: DbUpdateValue[R]
  ): DbUpdateValue[Lambda[A => L[A] :+: R[A]]] =
    new DbUpdateValue[Lambda[A => L[A] :+: R[A]]] {

      override def definitionResult[A](alg: L[A] :+: R[A]): (Index, Key) => DefinitionResult[A] =
        alg match {
          case Inl(l) => li.definitionResult(l)
          case Inr(r) => ri.definitionResult(r)
        }

    }

  implicit class InterpreterOps[ALG[_], OUT](val base: DbUpdateValue[ALG])
      extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: DbUpdateValue[R]
    ): DbUpdateValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }

  object CNilUpdateInterpreter extends DbUpdateValue[CNilF] {
    override def definitionResult[A](alg: CNilF[A]): (Index, Key) => DefinitionResult[A] =
      sys.error("Unreachable code")
  }
}

trait DbUpdateValue[ALG[_]] {
  def definitionResult[A](alg: ALG[A]): (Index, Key) => DefinitionResult[A]
}
