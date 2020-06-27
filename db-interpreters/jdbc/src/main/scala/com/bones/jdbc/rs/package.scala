package com.bones.jdbc

import java.sql.ResultSet

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.values.{DefaultValues, CNilF}
import com.bones.jdbc.FindInterpreter.{FieldName, Path}
import shapeless.{:+:, Coproduct, Inl, Inr}

package object rs {

  val defaultResultSetInterpreter: ResultSetValue[DefaultValues] =
    (DefaultScalaCoreResultSet ++
      (DefaultCustomStringResultSet ++
        (DefaultJavaTimeResultSet ++
          (DefaultJavaUtilResultSet ++ ResultSetValue.CNilResultSetValue))))

  object DefaultJavaTimeResultSet extends JavaTimeResultSet
  object DefaultJavaUtilResultSet extends JavaUtilResultSet
  object DefaultCustomStringResultSet extends CustomStringResultSetValue
  object DefaultScalaCoreResultSet extends ScalaCoreResultSet

  object ResultSetValue {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
                                                li: ResultSetValue[L],
                                                ri: ResultSetValue[R]
    ): ResultSetValue[Lambda[A => L[A] :+: R[A]]] =
      new ResultSetValue[Lambda[A => L[A] :+: R[A]]] {

        override def resultSet[A](alg: L[A] :+: R[A])
          : (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
          alg match {
            case Inl(l) => li.resultSet(l)
            case Inr(r) => ri.resultSet(r)
          }

      }

    implicit class InterpreterOps[ALG[_], OUT](val base: ResultSetValue[ALG])
        extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: ResultSetValue[R]
      ): ResultSetValue[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)

    }

    object CNilResultSetValue extends ResultSetValue[CNilF] {
      override def resultSet[A](
        alg: CNilF[A]): (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A] =
        sys.error("Unreachable")
    }
  }
  trait ResultSetValue[ALG[_]] {
    def resultSet[A](
      alg: ALG[A]): (Path, FieldName) => ResultSet => Either[NonEmptyList[ExtractionError], A]
  }

}
