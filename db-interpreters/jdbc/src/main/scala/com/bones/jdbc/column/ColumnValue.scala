package com.bones.jdbc.column

import com.bones.data.values.CNilF
import com.bones.jdbc.column.DbColumnInterpreter.ToColumns
import shapeless.{:+:, Coproduct, Inl, Inr}

object ColumnValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
                                              li: ColumnValue[L],
                                              ri: ColumnValue[R]
  ): ColumnValue[Lambda[A => L[A] :+: R[A]]] =
    new ColumnValue[Lambda[A => L[A] :+: R[A]]] {

      override def toColumns[B](alg: L[B] :+: R[B]): ToColumns = alg match {
        case Inl(l) => li.toColumns(l)
        case Inr(r) => ri.toColumns(r)
      }
    }

  implicit class InterpreterOps[ALG[_], OUT](val base: ColumnValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: ColumnValue[R]
    ): ColumnValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }

  object CNilColumnValue extends ColumnValue[CNilF] {
    override def toColumns[A](alg: CNilF[A]): ToColumns = sys.error("Unreachable code")
  }
}

trait ColumnValue[ALG[_]] {
  def toColumns[A](alg: ALG[A]): ToColumns
}
