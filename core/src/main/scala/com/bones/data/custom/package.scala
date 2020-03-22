package com.bones.data

import shapeless.ops.coproduct
import shapeless.{:+:, CNil, Inl, Inr}

package object custom {

  type CNilF[A] = CNil // trick to make things consistent on kind-level

  /* This Type ties all the custom algebras together into a single coproduct */
  type CustomStringCoproduct[A] = CustomStringValue[A] :+: CNilF[A]
  type AllCustomAlgebras[A] = JavaTimeValue[A] :+: CustomStringCoproduct[A]

  /** This is to allow smart constructors for each custom algebra, so that the data structure is lifted into the
    * context of AllCustomAlgebras type.  For example, the smart constructor `email` for creating an email data type would
    * */
  object AllCustomSyntax extends JavaTimeValueSugarInjected[AllCustomAlgebras] with CustomStringValueSugarInjected[AllCustomAlgebras] {

    override def javaTimeInject[A]: coproduct.Inject[AllCustomAlgebras[A], JavaTimeValue[A]] = Inl(_)

    override def stringValueInject[String]: coproduct.Inject[AllCustomAlgebras[String], CustomStringValue[String]] = i => Inr(Inl(i))
  }

}
