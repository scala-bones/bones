package com.bones.validation

import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error.ValidationError
import com.bones.validation.ValidationDefinition.ValidationOp

import scala.annotation.tailrec

object ValidationUtil {

  /** Validate the input with all specified validations.  If any failed then Left, else Right(input) */
  def validate[L](validations: List[ValidationOp[L]])(input: L, path: Vector[String]): Either[NonEmptyList[ValidationError[L]], L] = {
    validations.flatMap(validation => {
      if (validation.isValid(input)) None
      else Some(ValidationError(path, validation, input))
    }) match {
      case head :: tail => Left(NonEmptyList(head, tail))
      case _ => Right(input)
    }
  }


  private def digitToInt(x:Char): Int = x.toInt - '0'.toInt

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
