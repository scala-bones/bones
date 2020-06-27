package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{:+:, CNil, Coproduct, Generic}

sealed abstract class KvpCoproduct[ALG[_], C <: Coproduct] { self =>

  def :+:[B: Manifest](head: Either[KvpCollection[ALG, B], ALG[B]]): KvpSingleValueLeft[ALG, B, C] =
    KvpSingleValueLeft(head, this, manifest[B])

  def :<+:[B: Manifest](head: KvpCollection[ALG, B]): KvpSingleValueLeft[ALG, B, C] =
    KvpSingleValueLeft(Left(head), this, manifest[B])

  def :+>:[B: Manifest](head: ALG[B]): KvpSingleValueLeft[ALG, B, C] =
    KvpSingleValueLeft(Right(head), this, manifest[B])

  /** Convert a Coproduct into an object with validation on the object. */
  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[ALG, C, A] =
    KvpCoproductConvert[ALG, C, A](self, gen.from, gen.to, validation.toList)

  /** Convert a Coproduct into an object */
  def convert[A: Manifest](implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[ALG, C, A] =
    convert[A]()

}

case class KvpCoNil[ALG[_]]() extends KvpCoproduct[ALG, CNil]

/**
  *
  * @param kvpValue The head of the coproduct
  * @param kvpTail The rest of the coproduct
  * @param manifestL The manifest of the Left value, A
  * @tparam ALG The GADT context.
  * @tparam A The head (or most left part) of the coproduct (of the values of Coproduct, the one in particular this instance represents)
  * @tparam R The remaining part of the coproduct.  This class
  */
case class KvpSingleValueLeft[ALG[_], A, R <: Coproduct](
  kvpValue: Either[KvpCollection[ALG, A], ALG[A]],
  kvpTail: KvpCoproduct[ALG, R],
  manifestL: Manifest[A]
) extends KvpCoproduct[ALG, A :+: R] {}
