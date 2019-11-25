package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{:+:, CNil, Coproduct, Generic}

abstract class KvpCoproduct[C <: Coproduct] {

  def :+:[B:Manifest](head: KvpValue[B]): KvpSingleValueLeft[B,C] =
    KvpSingleValueLeft(head, this, manifest[B])

  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[C, A] =
    KvpCoproductConvert(this, gen.from, gen.to, validation.toList)


  def convert[A: Manifest](implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[C, A] = convert[A]()


}

case object KvpCoNil extends KvpCoproduct[CNil]

case class KvpSingleValueLeft[L, R<:Coproduct](
                                                kvpValue: KvpValue[L],
                                                kvpTail: KvpCoproduct[R],
                                                manifestL: Manifest[L]
  ) extends KvpCoproduct[L:+:R] {

}


