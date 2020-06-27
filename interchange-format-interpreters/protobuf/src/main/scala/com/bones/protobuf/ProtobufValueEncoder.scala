package com.bones.protobuf

import com.bones.data.values.CNilF
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.EncodeToProto
import shapeless.{:+:, Coproduct, Inl, Inr}

object ProtobufValueEncoder {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct, A](
    li: ProtobufValueEncoder[L],
    ri: ProtobufValueEncoder[R]): ProtobufValueEncoder[Lambda[A => L[A] :+: R[A]]] =
    new ProtobufValueEncoder[Lambda[A => L[A] :+: R[A]]] {
      override def encodeToProto[A](lr: L[A] :+: R[A]): EncodeToProto[A] = lr match {
        case Inl(l) => li.encodeToProto(l)
        case Inr(r) => ri.encodeToProto(r)
      }
    }

  implicit class InterpreterOps[ALG[_]](val base: ProtobufValueEncoder[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: ProtobufValueEncoder[R]): ProtobufValueEncoder[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)
  }

  object CNilProtobufValueEncoder extends ProtobufValueEncoder[CNilF] {
    override def encodeToProto[A](alg: CNilF[A]): EncodeToProto[A] = sys.error("Unreachable code")
  }
}

trait ProtobufValueEncoder[ALG[_]] {
  def encodeToProto[A](alg: ALG[A]): EncodeToProto[A]
}
