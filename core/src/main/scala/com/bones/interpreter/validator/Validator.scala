package com.bones.interpreter.validator

import com.bones.data.Error.ExtractionErrors

trait Validator[K, ALG[_], A, IN] { self =>
  def validate(in: IN): Either[ExtractionErrors[K], A] = validateWithPath(in, List.empty)
  def validateWithPath(in: IN, path: List[K]): Either[ExtractionErrors[K], A]

  def map[B](f: A => B): Validator[K, ALG, B, IN] =
    (in: IN, path: List[K]) => self.validateWithPath(in, path).map(f)

  def andThen[B, ALG2[_] <: ALG[_]](f: Validator[K, ALG2, B, A]): Validator[K, ALG2, B, IN] =
    (in: IN, path: List[K]) => {
      validateWithPath(in, path) match {
        case Left(errs) => Left(errs)
        case Right(a)   => f.validateWithPath(a, path)
      }
    }

}
