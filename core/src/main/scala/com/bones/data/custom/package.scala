package com.bones.data

import shapeless.ops.coproduct
import shapeless.{:+:, CNil, Inl, Inr}

package object custom {

  type CNilF[A] = CNil // trick to make things consistent on kind-level

  type AllCustomAlgebras[A] = JavaTimeValue[A] :+: CustomStringValue[A] :+: CNilF[A]

  object AllCustomSyntax extends JavaTimeValueSugarInjected[AllCustomAlgebras] with CustomStringValueSugarInjected[AllCustomAlgebras] {

    override def javaTimeInject[A]: coproduct.Inject[AllCustomAlgebras[A], JavaTimeValue[A]] = Inl(_)

    override def stringValueInject[String]: coproduct.Inject[AllCustomAlgebras[String], CustomStringValue[String]] = i => Inr(Inl(i))
  }

}
