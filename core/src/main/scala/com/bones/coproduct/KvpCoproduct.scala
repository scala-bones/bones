package com.bones.coproduct

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{:+:, CNil, Coproduct, Generic}

abstract class KvpCoproduct[C <: Coproduct, ALG[_]] { self =>

  def :+:[B:Manifest](head: Either[KvpValue[B], ALG[B]]): KvpSingleValueLeft[B,C, ALG] =
    KvpSingleValueLeft(head, this, manifest[B])

  def :+:[B:Manifest](head: KvpValue[B]): KvpSingleValueLeft[B,C,ALG] =
    KvpSingleValueLeft(Left(head), this, manifest[B])

  /** Convert a Coproduct into an object with validation on the object. */
  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[C, A, ALG] =
    KvpCoproductConvert[C, A, ALG](self, gen.from, gen.to, validation.toList)

  /** Convert a Coproduct into an object */
  def convert[A: Manifest](implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[C, A, ALG] = convert[A]()


}

case class KvpCoNil[ALG[_]]() extends KvpCoproduct[CNil, ALG]

case class KvpSingleValueLeft[A, R<:Coproduct, ALG[_]](
                                                kvpValue: Either[KvpValue[A], ALG[A]],
                                                kvpTail: KvpCoproduct[R, ALG],
                                                manifestL: Manifest[A]
  ) extends KvpCoproduct[A:+:R, ALG] {

}


