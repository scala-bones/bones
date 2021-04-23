package com.bones.cask.form

import com.bones.data.values.CNilF
import scalatags.Text
import shapeless.{:+:, Coproduct, Inl, Inr}
object FormElement {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
    * see https://stackoverflow.com/a/60561575/387094
    * */
  def merge[L[_], R[_] <: Coproduct](
    li: FormElement[L],
    ri: FormElement[R]
  ): FormElement[Lambda[A => L[A] :+: R[A]]] =
    new FormElement[Lambda[A => L[A] :+: R[A]]] {

      override def generateFormElement[A](
        alg: L[A] :+: R[A],
        path: List[String]): FormElementEncoder[A] = alg match {
        case Inl(l) => li.generateFormElement(l, path)
        case Inr(r) => ri.generateFormElement(r, path)
      }
    }

  implicit class InterpreterOps[ALG[_]](val base: FormElement[ALG]) extends AnyVal {
    def ++[R[_] <: Coproduct](
      r: FormElement[R]
    ): FormElement[Lambda[A => ALG[A] :+: R[A]]] =
      merge(base, r)

  }

  object CNilFormElement extends FormElement[CNilF] {

    override def generateFormElement[A](kvp: CNilF[A], path: List[String]): FormElementEncoder[A] =
      sys.error("Unreachable")
  }
}
trait FormElement[ALG[_]] {
  def generateFormElement[A](kvp: ALG[A], path: List[String]): FormElementEncoder[A]
}

trait FormElementEncoder[A] {
  def encodeForm(a: A): Text.TypedTag[String]
}
