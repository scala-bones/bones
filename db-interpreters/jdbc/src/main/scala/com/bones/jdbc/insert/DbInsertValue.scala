package com.bones.jdbc.insert

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object CNilInsertInterpreter extends DbInsertValue[CNilF] {
  override def insertPair[A](alg: CNilF[A]): InsertPair[A] =
    sys.error("Unreachable code")
}

object DbInsertValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct](
    li: DbInsertValue[L],
    ri: DbInsertValue[R]
  ): DbInsertValue[Lambda[A => L[A] :+: R[A]]] =
    new DbInsertValue[Lambda[A => L[A] :+: R[A]]] {

      override def insertPair[A](alg: L[A] :+: R[A]): InsertPair[A] =
        alg match {
          case Inl(l) => li.insertPair(l)
          case Inr(r) => ri.insertPair(r)
        }

    }

  implicit class InterpreterOps[ALG[_]](val base: DbInsertValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: DbInsertValue[R]
    ): DbInsertValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }

}

trait DbInsertValue[ALG[_]] {
  def insertPair[A](alg: ALG[A]): InsertPair[A]
}
