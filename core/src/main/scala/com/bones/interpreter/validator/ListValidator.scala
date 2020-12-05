package com.bones.interpreter.validator

import com.bones.data.Error.ExtractionErrors

import scala.annotation.tailrec

case class ListValidator[K, ALG[_], A, IN](
  singleElementValidator: Validator[K, ALG, A, IN],
  f: IN => Either[ExtractionErrors[K], List[IN]]
) extends Validator[K, ALG, List[A], IN] {
  override def validateWithPath(in: IN, path: List[K]): Either[ExtractionErrors[K], List[A]] =
    f(in).flatMap(failFast(path, _, List.empty).map(_.reverse))

  @tailrec
  private def failFast(
    path: List[K],
    list: List[IN],
    validated: List[A]
  ): Either[ExtractionErrors[K], List[A]] = {
    list match {
      case Nil => Right(validated)
      case x :: xs => {
        singleElementValidator.validateWithPath(x, path) match {
          case Left(errs) => Left(errs)
          case Right(a)   => failFast(path, xs, a :: validated)
        }
      }
    }
  }
}
