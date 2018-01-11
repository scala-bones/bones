package com.gaia.soy




object Transform {

//  implicit class TransformSyntax[F[_], A](val self: F[A])(implicit t: Transform[F]) {
//
//    /**
//      * Transforms using implicitly available evidence that such a transformation is possible.
//      *
//      * Typical transformations include converting:
//      *  - an `F[L]` for some `L <: HList` to/from an `F[CC]` for some case class `CC`, where the types in the case class are
//      *    aligned with the types in `L`
//      *  - an `F[C]` for some `C <: Coproduct` to/from an `F[SC]` for some sealed class `SC`, where the component types in
//      *    the coproduct are the leaf subtypes of the sealed class.
//      * @group combinators
//      */
//    def as[B](implicit as: Transformer[A, B]): F[B] = as(self)
//  }

}

abstract class Transform[F[_]] {
  self =>

  /**
    * Transforms supplied `F[A]` to an `F[B]` using two functions, `A => Attempt[B]` and `B => Attempt[A]`.
    */
  def xmap[A, B](fa: F[A], f: A => B, g: B => A): F[B]

}
