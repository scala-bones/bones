package com.bones.interpreter.deltavalidator

import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

trait InterchangeFormatDeltaValidatorValue[ALG[_], IN] {
  def createDeltaValidator[A](alg: ALG[A]): DeltaValueValidator[String, ALG, A, IN]
}

object InterchangeFormatDeltaValidatorValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters */
  def merge[L[_], R[_] <: Coproduct, IN](
    li: InterchangeFormatDeltaValidatorValue[L, IN],
    ri: InterchangeFormatDeltaValidatorValue[R, IN]
  ): InterchangeFormatDeltaValidatorValue[Lambda[A => L[A] :+: R[A]], IN] =
    new InterchangeFormatDeltaValidatorValue[Lambda[A => L[A] :+: R[A]], IN] {
      override def createDeltaValidator[AA](
        lr: L[AA] :+: R[AA]): DeltaValueValidator[String, Lambda[A => L[A] :+: R[A]], AA, IN] =
        lr match {
          case Inl(l) =>
            new DeltaValueValidator[String, Lambda[A => L[A] :+: R[A]], AA, IN] {
              val validator = li.createDeltaValidator(l)
              override def extract(
                in: IN,
                key: String): Either[ExtractionErrors[String], NullableResult[String, IN]] =
                validator.extract(in, key)

              override def validate(
                in: IN,
                path: List[String]): Either[ExtractionErrors[String], NullableResult[String, AA]] =
                validator.validate(in, path)
            }
          case Inr(r) =>
            new DeltaValueValidator[String, Lambda[A => L[A] :+: R[A]], AA, IN] {
              val validator = ri.createDeltaValidator(r)

              override def extract(
                in: IN,
                key: String): Either[ExtractionErrors[String], NullableResult[String, IN]] =
                validator.extract(in, key)

              override def validate(
                in: IN,
                path: List[String]): Either[ExtractionErrors[String], NullableResult[String, AA]] =
                validator.validate(in, path)
            }
        }
    }

  implicit class InterpreterOps[ALG[_], IN](val base: InterchangeFormatDeltaValidatorValue[ALG, IN])
      extends AnyVal {
    def ++[R[_] <: Coproduct](r: InterchangeFormatDeltaValidatorValue[R, IN])
      : InterchangeFormatDeltaValidatorValue[Lambda[A => ALG[A] :+: R[A]], IN] =
      merge(base, r)

  }

  case class CNilInterchangeFormatValidator[IN]()
      extends InterchangeFormatDeltaValidatorValue[CNilF, IN] {
    override def createDeltaValidator[A](alg: CNilF[A]): DeltaValueValidator[String, CNilF, A, IN] =
      sys.error("Unreachable code")
  }
}
