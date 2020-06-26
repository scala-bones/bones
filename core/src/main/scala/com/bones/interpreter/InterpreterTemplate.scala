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
      case nil: KvpNil[_]                             => ???
      case op: KvpSingleValueHead[alg, h, t, tl, a]   => ???
      case op: KvpHListHead[alg, a, al, h, hl, t, tl] => ???
      case op: KvpConcreteTypeHead[alg, a, ht, nt]    => ???
    }
  }

  def kvpCoproduct[ALG[_], C <: Coproduct](co: KvpCoproduct[ALG, C]): Unit = {
    co match {
      case nil: KvpCoNil[_]                  => ???
      case co: KvpSingleValueLeft[alg, l, r] => ???
    }
  }

  def determineValueDefinition[ALG[_], A](
                                           value: Either[KvpCollection[ALG,A], ALG[A]],
                                           interpreter: Nothing): Unit = ???

  def valueDefinition[ALG[_], A](fgo: KvpCollection[ALG,A]): Unit =
    fgo match {
      case op: OptionalKvpValueDefinition[alg, a] => ???
      case ld: ListData[alg, t]                   => ???
      case ed: EitherData[alg, a, b]              => ???
      case kvp: KvpHListValue[alg, h, hl]         => ???
      case co: KvpCoproductValue[alg, c]          => ???
      case x: HListConvert[alg, a, al, b]         => ???
      case co: KvpCoproductConvert[alg, c, a]     => ???
    }

}
