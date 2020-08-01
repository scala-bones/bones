package com.bones.liquibase

import com.bones.data.values.CNilF
import com.bones.liquibase.GenerateDatabaseObject.{Name, TableItems}
import liquibase.structure.core.Table
import shapeless.{:+:, Coproduct, Inl, Inr}

object DatabaseObjectValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
    li: DatabaseObjectValue[L],
    ri: DatabaseObjectValue[R]
  ): DatabaseObjectValue[Lambda[A => L[A] :+: R[A]]] =
    new DatabaseObjectValue[Lambda[A => L[A] :+: R[A]]] {
      override def databaseObject[B](
        alg: L[B] :+: R[B]): Name => (List[(Table, TableItems)], List[TableItems]) =
        alg match {
          case Inl(l) => li.databaseObject(l)
          case Inr(r) => ri.databaseObject(r)
        }

    }

  implicit class InterpreterOps[ALG[_], OUT](val base: DatabaseObjectValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: DatabaseObjectValue[R]
    ): DatabaseObjectValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }
}

object CNilDatabaseObjectValue extends DatabaseObjectValue[CNilF] {
  override def databaseObject[A](
    alg: CNilF[A]): Name => (List[(Table, TableItems)], List[TableItems]) =
    _ => sys.error("Unreachable code")
}

trait DatabaseObjectValue[ALG[_]] {
  def databaseObject[A](alg: ALG[A]): Name => (List[(Table, TableItems)], List[TableItems])
}
