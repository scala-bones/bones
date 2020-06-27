package com.bones.data

import shapeless.ops.coproduct
import shapeless.{:+:, CNil, Inl, Inr}

package object values {

  type CNilF[A] = CNil // trick to make things consistent on kind-level
  type AnyAlg[_] = Any // when an algebra interpreter can accept any algebra (for instance when the output is based on the keys and not the values)


  /* This Type ties all the algebras together into a single coproduct */
  type DefaultValues[A] =
    ScalaCoreValue[A] :+: CustomStringValue[A] :+: JavaTimeValue[A] :+: JavaUtilValue[A] :+:
      CNilF[A]

  /** This is to allow smart constructors for each GADT, so that the data structure is lifted into the
    * context of [[DefaultValues]] type.  For example, the smart constructor `email` for creating an email data type would
    *  become Inr(Inl(EmailData()) which satisfies the [[DefaultValues]] definition.
    * */
  trait DefaultValuesSyntax
      extends ScalaCoreInjectedSugar[DefaultValues]
      with CustomStringValueSugarInjected[DefaultValues]
      with JavaTimeValueSugarInjected[DefaultValues]
      with JavaUtilInjectedSugar[DefaultValues] {

    override def scalaCoreInjected[A]: coproduct.Inject[DefaultValues[A], ScalaCoreValue[A]] =
      Inl(_)

    override def stringValueInject[String]
      : coproduct.Inject[DefaultValues[String], CustomStringValue[String]] = i => Inr(Inl(i))

    override def javaTimeInject[A]: coproduct.Inject[DefaultValues[A], JavaTimeValue[A]] =
      i => Inr(Inr(Inl(i)))

    override def javaUtilInjected[A]: coproduct.Inject[DefaultValues[A], JavaUtilValue[A]] =
      i => Inr(Inr(Inr(Inl(i))))

  }

}
