package com.bones.jdbc

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.custom.{AllCustomAlgebras, CNilF}
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import shapeless.{:+:, Coproduct, Inl, Inr}

package object rs {

  val defaultResultSetInterpreter: ResultSetValueInterpreter[AllCustomAlgebras] =
    (DefaultScalaCoreResultSetInterpreter ++
      (DefaultCustomStringResultSetInterpreter ++
        (DefaultJavaTimeResultSetInterpreter ++
          (DefaultJavaUtilResultSetInterpreter ++ ResultSetValueInterpreter.CNilResultSetValueInterpreter))))

  object DefaultJavaTimeResultSetInterpreter extends JavaTimeResultSetInterpreter
  object DefaultJavaUtilResultSetInterpreter extends JavaUtilResultSetInterpreter
  object DefaultCustomStringResultSetInterpreter extends CustomStringResultSetValueInterpreter
  object DefaultScalaCoreResultSetInterpreter extends ScalaCoreResultSetInterpreter

  object ResultSetValueInterpreter {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
      li: ResultSetValueInterpreter[L],
      ri: ResultSetValueInterpreter[R]
    ): ResultSetValueInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new ResultSetValueInterpreter[Lambda[A => L[A] :+: R[A]]] {

        override def resultSet[A](alg: L[A] :+: R[A])
          : (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
          alg match {
            case Inl(l) => li.resultSet(l)
            case Inr(r) => ri.resultSet(r)
          }

      }

    implicit class InterpreterOps[ALG[_], OUT](val base: ResultSetValueInterpreter[ALG])
        extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: ResultSetValueInterpreter[R]
      ): ResultSetValueInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)

    }

    object CNilResultSetValueInterpreter extends ResultSetValueInterpreter[CNilF] {
      override def resultSet[A](
        alg: CNilF[A]): (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
        sys.error("Unreachable")
    }
  }
  trait ResultSetValueInterpreter[ALG[_]] {
    def resultSet[A](
      alg: ALG[A]): (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A]
  }

}
