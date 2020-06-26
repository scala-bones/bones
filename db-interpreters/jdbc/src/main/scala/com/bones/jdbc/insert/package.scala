package com.bones.jdbc

import com.bones.data.custom.{AllCustomAlgebras, CNilF}
import com.bones.jdbc.insert.DbInsertValues.InsertPair
import shapeless.{:+:, Coproduct, Inl, Inr}

package object insert {

  val defaultDbInsertInterpreter: CustomInterpreter[AllCustomAlgebras] =
    DefaultScalaCoreDbInsert ++
      (DefaultCustomStringDbInsert ++
        (DefaultJavaTimeDbInsert ++
          (DefaultJavaUtilDbInsert ++ CNilUpdateInterpreter)))

  object DefaultScalaCoreDbInsert extends ScalaCoreDbInsert
  object DefaultJavaUtilDbInsert extends JavaUtilDbInsert
  object DefaultJavaTimeDbInsert extends JavaTimeDbInsert
  object DefaultCustomStringDbInsert extends CustomStringDbInsert

  object CNilUpdateInterpreter extends CustomInterpreter[CNilF] {
    override def insertPair[A](alg: CNilF[A]): InsertPair[A] =
      sys.error("Unreachable code")
  }

  object CustomInterpreter {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
      li: CustomInterpreter[L],
      ri: CustomInterpreter[R]
    ): CustomInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new CustomInterpreter[Lambda[A => L[A] :+: R[A]]] {

        override def insertPair[A](alg: L[A] :+: R[A]): InsertPair[A] =
          alg match {
            case Inl(l) => li.insertPair(l)
            case Inr(r) => ri.insertPair(r)
          }

      }

    implicit class InterpreterOps[ALG[_], OUT](val base: CustomInterpreter[ALG]) extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: CustomInterpreter[R]
      ): CustomInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)

    }

  }

  trait CustomInterpreter[ALG[_]] {
    def insertPair[A](alg: ALG[A]): InsertPair[A]
  }

}
