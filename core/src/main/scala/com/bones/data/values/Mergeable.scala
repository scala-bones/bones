package com.bones.data.values

import cats.{Applicative, Apply}
import shapeless.{:+:, Inl, Inr}

/**
 *
 * @tparam F
 */
trait Mergeable[F[_[_]]] {

//  def merge[L[_],R[_],A, O](l: F[L[A]], r: F[R[A]], f: A => O)(
//    implicit A: Applicative[F]
//  ): F[Lambda[A => L[A] :+: R[A]]] = {
//
//    val f: L[A] :+: R[A] =>  Lambda[A => L[A] :+: R[A]] = {
//      case Inl(head) => A.pure(head)
//      case Inr(tail) => ???
//    }
//
//    A.ap[L[A] :+: R[A], Lambda[A => L[A] :+: R[A]]]()
//    alg match {
//      case Inl(head) => A.ap[]
//      case Inr(tail) => ???
//    }
//  }

}
