package com.bones.protobuf

import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object ProtobufValidatorValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct](
    li: ProtobufValidatorValue[L],
    ri: ProtobufValidatorValue[R]): ProtobufValidatorValue[Lambda[A => L[A] :+: R[A]]] =
    new ProtobufValidatorValue[Lambda[A => L[A] :+: R[A]]] {
      override def extractFromProto[A](lr: L[A] :+: R[A]): ExtractFromProto[A] = lr match {
        case Inl(l) => li.extractFromProto(l)
        case Inr(r) => ri.extractFromProto(r)
      }
    }

  implicit class InterpreterOps[ALG[_]](val base: ProtobufValidatorValue[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: ProtobufValidatorValue[R]): ProtobufValidatorValue[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  object CNilProtobufValueValidator extends ProtobufValidatorValue[CNilF] {
    override def extractFromProto[A](alg: CNilF[A]): ExtractFromProto[A] =
      sys.error("Unreachable code")
  }
}

trait ProtobufValidatorValue[ALG[_]] {
  def extractFromProto[A](alg: ALG[A]): ExtractFromProto[A]
}
