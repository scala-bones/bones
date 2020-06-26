package com.bones.data

import shapeless.ops.coproduct
import shapeless.{:+:, CNil, Inl, Inr}

package object custom {

  type CNilF[A] = CNil // trick to make things consistent on kind-level
  type AnyAlg[_] = Any // when an algebra interpreter can accept any algebra (for instance when the output is based on the keys and not the values)


  /* This Type ties all the algebras together into a single coproduct */
  type AllCustomAlgebras[A] =
    ScalaCoreValue[A] :+: CustomStringValue[A] :+: JavaTimeValue[A] :+: JavaUtilValue[A] :+:
      CNilF[A]

  /** This is to allow smart constructors for each custom algebra, so that the data structure is lifted into the
    * context of AllCustomAlgebras type.  For example, the smart constructor `email` for creating an email data type would
    *  become Inr(Inl(EmailData()) which satisfies the AllCustomAlgebras definition.
    * */
  trait AllCustomSyntax
      extends ScalaCoreInjectedSugar[AllCustomAlgebras]
      with CustomStringValueSugarInjected[AllCustomAlgebras]
      with JavaTimeValueSugarInjected[AllCustomAlgebras]
      with JavaUtilInjectedSugar[AllCustomAlgebras] {

    override def scalaCoreInjected[A]: coproduct.Inject[AllCustomAlgebras[A], ScalaCoreValue[A]] =
      Inl(_)

    override def stringValueInject[String]
      : coproduct.Inject[AllCustomAlgebras[String], CustomStringValue[String]] = i => Inr(Inl(i))

    override def javaTimeInject[A]: coproduct.Inject[AllCustomAlgebras[A], JavaTimeValue[A]] =
      i => Inr(Inr(Inl(i)))

    override def javaUtilInjected[A]: coproduct.Inject[AllCustomAlgebras[A], JavaUtilValue[A]] =
      i => Inr(Inr(Inr(Inl(i))))

  }

}
