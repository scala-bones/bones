package com.ot.bones.validation

import cats.Id
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.ot.bones.interpreter.ExtractionInterpreter.{AppendGenericValidation, ExtractionError, ExtractionErrors, JsonProducer, RequiredOp, ValidationError, ValidationOp}


object IntValidation {

  case class Between(min: Int, max: Int) extends ValidationOp[Int] {
    val isValid: Int => Boolean = input => input >= min && input <= max

    override def defaultError(t: Int): String = s"$t is not between $min and $max"

    override def description: String = s"between $min and $max"

  }

  case class Max(maxInt: Int) extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ <= maxInt

    override def defaultError(t: Int): String = s"$t is greater than $maxInt"

    override def description: String = s"maximum of $maxInt"
  }

  case class Min(minInt: Int) extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ >= minInt

    override def defaultError(t: Int): String = s"$t is less than $minInt"

    override def description: String = s"minimum of $minInt"
  }

  case class Greater(greaterThan: Int) extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ > greaterThan

    override def defaultError(t: Int): String = s"$t is not greater than $greaterThan"

    override def description: String = s"greater than $greaterThan"
  }

  case class Less(lessThan: Int) extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ < lessThan

    override def defaultError(t: Int): String = s"$t is not less than $lessThan"

    override def description: String = s"less than $lessThan"
  }

  case class Multiple(multipleOf: Int) extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ % multipleOf === 0

    override def defaultError(t: Int): String = s"$t is not a multiple of $multipleOf"

    override def description: String = s"multiple of $multipleOf"
  }

  case class Positive() extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ >= 0

    override def defaultError(t: Int): String = s"$t is not positive"

    override def description: String = s"positive"
  }

  case class Negative() extends ValidationOp[Int] {
    override def isValid: Int => Boolean = _ <= 0

    override def defaultError(t: Int): String = s"$t is not negative"

    override def description: String = s"negative"
  }



  /**
    * This is for the syntactic sugar of adding validation types.
    *
    * @tparam NSE The 'Next StringExtraction' type if a new value is appended.
    */
  trait IntExtraction[F[_], NSE] extends AppendGenericValidation[Int, F, NSE] {

    def between(min: Int, max: Int): NSE = append(Between(min,max))
    def max(maxValue: Int): NSE = append(Max(maxValue))
    def min(minValue: Int): NSE = append(Min(minValue))
    def greater(value: Int): NSE = append(Greater(value))
    def less(value: Int): NSE = append(Less(value))



  }


  /**
    * FieldGroup Operation declares that a key is an optional string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */

  final case class OptionalInt(key: Key, validations: List[ValidationOp[Int]]) extends DataDefinitionOp[Option[Int]]
    with IntExtraction[Option, OptionalInt] {

    def extract(input: JsonProducer): Validated[ExtractionErrors, Option[Int]] = {
      input.produceInt(key).toValidatedNel.andThen {
        case Some(int) =>
          ValidationUtil.runAndMapValidations(key, int, validations).map(Some(_))
        case None => Valid(None)
      }
    }


    def append(sv: ValidationOp[Int]): OptionalInt = OptionalInt(key, sv :: validations)
  }

  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */
  final case class RequiredInt(key: Key, validations: List[ValidationOp[Int]]) extends DataDefinitionOp[Int]
    with IntExtraction[Id, RequiredInt] {

    def optional(): OptionalInt = OptionalInt(key, validations)

    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Int] = {
      producer.produceInt(key).toValidated.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(ValidationError(key, RequiredOp(), None)))
      }.andThen(i =>
        ValidationUtil.runAndMapValidations(key, i, validations)
      )
    }

    def append(sv: ValidationOp[Int]): RequiredInt = RequiredInt(key, sv :: validations)

  }

}
