package com.bones.protobuf

import com.bones.data.values.CNilF
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.ExtractFromProto
import shapeless.{:+:, Coproduct, Inl, Inr}

object ProtobufValueValidator {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
   * see https://stackoverflow.com/a/60561575/387094
   * */
  def merge[L[_], R[_] <: Coproduct, A](
                                         li: ProtobufValueValidator[L],
                                         ri: ProtobufValueValidator[R]): ProtobufValueValidator[Lambda[A => L[A] :+: R[A]]] =
    new ProtobufValueValidator[Lambda[A => L[A] :+: R[A]]] {
      override def extractFromProto[A](lr: L[A] :+: R[A]): ExtractFromProto[A] = lr match {
        case Inl(l) => li.extractFromProto(l)
        case Inr(r) => ri.extractFromProto(r)
      }
    }

  implicit class InterpreterOps[ALG[_]](val base: ProtobufValueValidator[ALG])
    extends AnyVal {
    def ++[R[_] <: Coproduct](r: ProtobufValueValidator[R])
    : ProtobufValueValidator[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  object CNilProtobufValueValidator extends ProtobufValueValidator[CNilF] {
    override def extractFromProto[A](alg: CNilF[A]): ExtractFromProto[A] =
      sys.error("Unreachable code")
  }
}

trait ProtobufValueValidator[ALG[_]] {
  def extractFromProto[A](alg: ALG[A]): ExtractFromProto[A]
}
