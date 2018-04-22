package com.bones.validation

import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.FieldDefinition
import com.bones.interpreter.ExtractionInterpreter.{FieldError, JsonProducer, ValidateFromProducer, ValidationError, ValidationResultNel}
import com.bones.validation.ValidationDefinition.ValidationOp

import scala.annotation.tailrec

object ValidationUtil {

  type ValidationResult[A] = A => Validated[ValidationError[A],A]
  object validationInterpreter extends FunctionK[ValidationOp, ValidationResult] {
    override def apply[A](fa: ValidationOp[A]): ValidationResult[A] = a => runValidation(a, fa)
  }

  /** Produce and validate -- produce the value from the specified key, then run through the validations of the FieldDef */
  def pv[X](jsonProducer: JsonProducer, fieldDefinition: FieldDefinition[X], fromProducer: ValidateFromProducer[X]) : ValidationResultNel[X] = {
    fromProducer(jsonProducer.child(fieldDefinition.key)).andThen { input =>
      ValidationUtil.validate(input, fieldDefinition.validations)
    }.leftMap(errors => NonEmptyList.one(FieldError(fieldDefinition.key, errors)))
  }



  def runValidation[T](input: T, validation: ValidationOp[T]): Validated[ValidationError[T], T] = {
    if (validation.isValid(input)) {
      Valid(input)
    } else {
      Invalid(ValidationError(validation,input))
    }
  }

  /** Validate the input with the specified validations.  If any failed then Invalid, else Valid */
  def validate[L](input: L, validations: List[ValidationOp[L]]): Validated[NonEmptyList[ValidationError[L]], L] = {
    validations.flatMap(validation => {
      if (validation.isValid(input)) None
      else Some(ValidationError(validation, input))
    }) match {
      case head :: tail => Invalid(NonEmptyList(head, tail))
      case _ => Valid(input)
    }
  }


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
