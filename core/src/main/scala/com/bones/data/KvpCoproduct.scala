package com.bones.data

import com.bones.data.Value.KvpValue
import shapeless.{:+:, CNil, Coproduct}

abstract class KvpCoproduct[C <: Coproduct] {

  def :+:[B:Manifest](head: KvpValue[B]): KvpSingleValueLeft[B,C] =
    KvpSingleValueLeft(head, this, manifest[B])

}

case object KvpCoNil extends KvpCoproduct[CNil]

case class KvpSingleValueLeft[L, R<:Coproduct](
  kvpValue: KvpValue[L],
  kvpTail: KvpCoproduct[R],
  manifestH: Manifest[L]
  ) extends KvpCoproduct[L:+:R] {

}

//case class KvpCoproductHead[A, H<:Coproduct, T<: Coproduct](head: KvpCoproduct[A,H], tail: KvpCoproduct[A,T]) extends KvpCoproduct[H :+: T] {
//
//}

