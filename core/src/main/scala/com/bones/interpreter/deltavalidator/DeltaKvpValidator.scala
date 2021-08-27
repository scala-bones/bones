package com.bones.interpreter.deltavalidator

import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors
import shapeless.{HList, UnaryTCConstraint}

trait DeltaKvpValidator[K, ALG[_], H <: HList, IN] { self =>

  def hListRR: UnaryTCConstraint[H, CanBeOmitted[String, *]]

  def validate(in: IN, path: List[String]): Either[ExtractionErrors[K], H]

  /** Adds another layer of validation. This validation will execute after any prior validation
    * (monad style, not applicative). This will only validate if there are not already any errors
    * and the value is non-null. NullableResults are simply passed through unvalidated.
    */
  def addValidation(
    f: (H, List[String]) => Either[ExtractionErrors[K], H]
  ): DeltaKvpValidator[K, ALG, H, IN] =
    new DeltaKvpValidator[K, ALG, H, IN] {
      override def hListRR: UnaryTCConstraint[H, CanBeOmitted[String, *]] = self.hListRR

      override def validate(in: IN, path: List[String]): Either[ExtractionErrors[K], H] =
        self.validate(in, path).flatMap(h => f(h, path))
    }

//  def map[B](f: Poly)(implicit mapper: Mapper[Poly,H]): DeltaKvpValidator[K, ALG, B, IN] = new DeltaKvpValidator[K, ALG, B, IN] {
//    override def validate(
//      in: IN,
//      path: List[String]): Either[ExtractionErrors[K], NullableResult[K, B]] = {
//      self.validate(in, path).map(_.map(f))
//    }
//  }
}
