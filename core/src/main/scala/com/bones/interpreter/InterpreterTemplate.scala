package com.bones.interpreter

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data._
import shapeless.{Coproduct, HList, Nat}

/**
  * Just a template to be used as a starting point for a new interpreter.
  * You can copy/paste this for a starting point for a new interpreter.
  */
class InterpreterTemplate {

  def kvpHList[ALG[_], H <: HList, HL <: Nat](group: KvpHList[ALG, H, HL]): Unit = {
    group match {
      case nil: KvpNil[_] => ???
      case op: KvpSingleValueHead[alg, h, t, tl, a] => ???
      case op: KvpHListHead[alg, a, al, h, hl, t, tl] => ???
      case op: KvpConcreteTypeHead[alg, a, ht, nt] => ???
    }
  }

  def kvpCoproduct[ALG[_], C<:Coproduct](co: KvpCoproduct[ALG, C]): Unit = {
    co match {
      case nil: KvpCoNil[_] => ???
      case co: KvpSingleValueLeft[alg, l,r] => ???
    }
  }

  def determineValueDefinition[ALG[_], A](value: Either[KvpValue[A], ALG[A]], interpreter: Nothing): Unit = ???

  def valueDefinition[A](fgo: KvpValue[A]): Unit =
    fgo match {
      case op: OptionalKvpValueDefinition[alg, a] => ???
      case ob: BooleanData => ???
      case rs: StringData => ???
      case id: IntData => ???
      case ri: LongData => ???
      case uu: UuidData => ???
      case dd: LocalDateTimeData => ???
      case ld: LocalDateData => ???
      case fd: FloatData => ???
      case sd: ShortData => ???
      case id: DoubleData => ???
      case bd: BigDecimalData => ???
      case ld: ListData[alg, t] => ???
      case ed: EitherData[alg, a, b] => ???
      case ba: ByteArrayData => ???
      case esd: EnumerationData[a,b] => ???
      case kvp: KvpHListValue[alg, h, hl] => ???
      case co: KvpCoproductValue[alg, c] => ???
      case x: HListConvert[alg, a, al, b] => ???
      case co: KvpCoproductConvert[alg, c,a] => ???
    }

}
