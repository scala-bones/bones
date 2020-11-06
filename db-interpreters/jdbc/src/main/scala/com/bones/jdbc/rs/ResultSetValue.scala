package com.bones.jdbc.rs

import java.sql.ResultSet

import com.bones.Path
import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.CNilF
import com.bones.jdbc.FindInterpreter.FieldName
import shapeless.{:+:, Coproduct, Inl, Inr}

object ResultSetValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
    li: ResultSetValue[L],
    ri: ResultSetValue[R]
  ): ResultSetValue[Lambda[A => L[A] :+: R[A]]] =
    new ResultSetValue[Lambda[A => L[A] :+: R[A]]] {

      override def resultSet[A](alg: L[A] :+: R[A]): (
        Path[String],
        FieldName) => ResultSet => Either[ExtractionErrors[String], NullableResult[String, A]] =
        alg match {
          case Inl(l) => li.resultSet(l)
          case Inr(r) => ri.resultSet(r)
        }

    }

  implicit class InterpreterOps[ALG[_], OUT](val base: ResultSetValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: ResultSetValue[R]
    ): ResultSetValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }

  object CNilResultSetValue extends ResultSetValue[CNilF] {
    override def resultSet[A](alg: CNilF[A]): (
      Path[String],
      FieldName) => ResultSet => Either[ExtractionErrors[String], NullableResult[String, A]] =
      sys.error("Unreachable")
  }
}
trait ResultSetValue[ALG[_]] {
  def resultSet[A](alg: ALG[A]): (
    Path[String],
    FieldName) => ResultSet => Either[ExtractionErrors[String], NullableResult[String, A]]
}
