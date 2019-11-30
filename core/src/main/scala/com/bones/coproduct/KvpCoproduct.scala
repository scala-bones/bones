package com.bones.coproduct

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{:+:, CNil, Coproduct, Generic}

abstract class KvpCoproduct[C <: Coproduct, COV[_]] { self =>

  def :+:[B:Manifest](head: Either[KvpValue[B], COV[B]]): KvpSingleValueLeft[B,C, COV] =
    KvpSingleValueLeft(head, this, manifest[B])

  def :+:[B:Manifest](head: KvpValue[B]): KvpSingleValueLeft[B,C,COV] =
    KvpSingleValueLeft(Left(head), this, manifest[B])

  def convert[A: Manifest](validation: ValidationOp[A]*)(
    implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[C, A, COV] =
    KvpCoproductConvert[C, A, COV](self, gen.from, gen.to, validation.toList)


  def convert[A: Manifest](implicit gen: Generic.Aux[A, C]): KvpCoproductConvert[C, A, COV] = convert[A]()


}

case class KvpCoNil[COV[_]]() extends KvpCoproduct[CNil, COV]

case class KvpSingleValueLeft[A, R<:Coproduct, COV[_]](
                                                kvpValue: Either[KvpValue[A], COV[A]],
                                                kvpTail: KvpCoproduct[R, COV],
                                                manifestL: Manifest[A]
  ) extends KvpCoproduct[A:+:R, COV] {

}


