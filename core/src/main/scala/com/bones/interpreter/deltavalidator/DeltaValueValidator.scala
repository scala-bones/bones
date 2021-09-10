package com.bones.interpreter.deltavalidator

import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors

object DeltaValueValidator {
  def pure[K, ALG[_], A, IN](na: CanBeOmitted[K, A]): DeltaValueValidator[K, ALG, A, IN] =
    (_: IN, _: K, _: List[K]) => Right(na)

  def pureA[K, ALG[_], A, IN](a: A): DeltaValueValidator[K, ALG, A, IN] =
    (_: IN, _: K, _: List[K]) => {
      Right(Right(a))
    }
}

/** Responsible for being able to extract and validate a value.
  * @tparam K
  *   The key type
  * @tparam ALG
  *   The Algebra
  * @tparam A
  *   The result if validation is successful
  * @tparam IN
  *   The input interchange format, such as a JSON blob.
  */
trait DeltaValueValidator[K, ALG[_], A, IN] { self =>

  def extract(in: IN, key: K, path: List[K]): Either[ExtractionErrors[K], CanBeOmitted[K, A]]

  /** Adds another layer of validation. This validation will execute after any prior validation
    * (monad style, not applicative). This will only validate if there are not already any errors
    * and the value is non-null. NullableResults are simply passed through unvalidated.
    */
  def addValidation(
    f: (A, List[K]) => Either[ExtractionErrors[K], A]
  ): DeltaValueValidator[K, ALG, A, IN] =
    (in: IN, key: K, path: List[K]) =>
      self.extract(in, key, path).flatMap {
        case Left(omitted) => Right(Left(omitted))
        case Right(a)      => f(a, path :+ key).map(Right(_))
      }

  def map[B](f: CanBeOmitted[K, A] => CanBeOmitted[K, B]): DeltaValueValidator[K, ALG, B, IN] =
    (in: IN, key: K, path: List[K]) => {
      self.extract(in, key, path).map(nr => f(nr))
    }

  def flatMap[B](
    f: CanBeOmitted[K, A] => DeltaValueValidator[K, ALG, B, IN]
  ): DeltaValueValidator[K, ALG, B, IN] =
    (in: IN, key: K, path: List[K]) => {
      self.extract(in, key, path) match {
        case Left(err) => Left(err)
        case Right(v) =>
          f(v).extract(in, key, path)
      }
    }

  def mapA[B](f: A => B): DeltaValueValidator[K, ALG, B, IN] =
    (a: IN, key: K, path: List[K]) => {
      self.extract(a, key, path).map(_.map(a => f(a)))
    }

  def flatMapA[B](f: A => DeltaValueValidator[K, ALG, B, IN]): DeltaValueValidator[K, ALG, B, IN] =
    (in: IN, key: K, path: List[K]) => {
      self.extract(in, key, path) match {
        case Left(err) => Left(err)
        case Right(v) =>
          v match {
            case Left(n) => Right(Left(n))
            case Right(a) =>
              val bResult = f(a)
              bResult.extract(in, key, path)
          }
      }
    }

  def ap[B](v: DeltaValueValidator[K, ALG, A => B, IN]): DeltaValueValidator[K, ALG, B, IN] =
    (in: IN, key: K, path: List[K]) => {
      self.extract(in, key, path) match {
        case Left(err) => Left(err)
        case Right(nv) =>
          nv match {
            case Left(n) => Right(Left(n))
            case Right(a) =>
              v.extract(in, key, path) match {
                case Left(err) => Left(err)
                case Right(nv) =>
                  nv match {
                    case Left(nv) => Right(Left(nv))
                    case Right(f) => Right(Right(f(a)))
                  }
              }
          }
      }
    }

}
