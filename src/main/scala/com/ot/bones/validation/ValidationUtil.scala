package com.ot.bones.validation

import cats.Apply
import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import com.ot.bones.interpreter.ExtractionInterpreter.{ValidationError, ValidationResultNel}
import cats.implicits._
import com.ot.bones.validation.ValidationDefinition.ValidationOp

import scala.annotation.tailrec

object ValidationUtil {

  type ValidationResult[A] = A => Validated[ValidationError[A],A]
  object validationInterpreter extends FunctionK[ValidationOp, ValidationResult] {
    override def apply[A](fa: ValidationOp[A]): ValidationResult[A] = a => runValidation(a, fa)
  }

  def runValidation[T](input: T, validation: ValidationOp[T]): Validated[ValidationError[T], T] = {
    if (validation.isValid(input)) {
      Valid(input)
    } else {
      Invalid(ValidationError(validation,input))
    }
  }

  /**
    * Responsible for running the list of validations.
    */
  def runAndMapValidations[T](input: T, validations: List[ValidationOp[T]])(implicit A: Apply[ValidatedNel[ValidationError[T], ?]]): ValidationResultNel[T] =
    validations.map(ValidationUtil.runValidation(input, _))
      .foldLeft[ValidatedNel[ValidationError[T], T]](Valid(input))((last, next) => {
        val n: ValidatedNel[ValidationError[T], T] = next.leftMap(NonEmptyList.one)
        A.map2(last, n)((a, b) => b)
      })

  def digitToInt(x:Char): Int = x.toInt - '0'.toInt

  /** True if the string passes the Luhn algorithm using the specified mod variable. */
  def luhnCheck(mod: Int, str: String): Boolean = {
    str.reverse.toList match {
      case x :: xs => (luhnSum(xs, 0, 2) * 9) % mod === digitToInt(x)
      case _ => false
    }
  }

  /** Calculates the total sum of the characters using the Luhn algorithm. */
  @tailrec
  def luhnSum(str: List[Char], sum: Int, multiplier: Int) : Int = {
    def nextMulti(m: Int) = if (m == 1) 2 else 1
    def doubleSum(i: Int) = i % 10 + i / 10
    str match {
      case Nil => sum
      case x :: xs => luhnSum(xs, sum + doubleSum(digitToInt(x) * multiplier), nextMulti(multiplier))
    }
  }
}
