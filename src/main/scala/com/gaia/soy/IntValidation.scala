package com.gaia.soy

import cats.Id
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import com.gaia.soy.validation.ValidationUtil


object IntValidation {

  case class Between(min: Int, max: Int) extends ValidationOp[Int] {
    val isValid: Int => Boolean = input => input >= min && input <= max

    override def defaultError(t: Int): String = s"${t} is not between $min and $max"

    override def description: String = s"between $min and $max"

  }



  /**
    * This is for the syntactic sugar of adding validation types.
    *
    * @tparam NSE The 'Next StringExtraction' type if a new value is appended.
    */
  trait IntExtraction[F[_], NSE] extends ExtractionAppend[Int, F, NSE] {

    def between(min: Int, max: Int): NSE = append(Between(min,max))

  }


  /**
    * FieldGroup Operation declares that a key is an optional string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */

  final case class OptionalInt(key: Key, validations: List[ValidationOp[Int]]) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], Option[Int]]]
    with IntExtraction[Option, OptionalInt] {

    def extract(input: JsonProducer): Validated[ExtractionErrors, Option[Int]] = {
      input.produceInt(key).toValidatedNel.andThen {
        case Some(int) => {
          ValidationUtil.runAndMapValidations(key, int, validations).map(Some(_))
        }
        case None => Valid(None)
      }
    }


    override def appendMetadata(md: Metadata): OptionalInt = ???

    def append(sv: ValidationOp[Int]): OptionalInt = OptionalInt(key, sv :: validations)
  }

  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */
  final case class RequiredInt(key: Key, validations: List[ValidationOp[Int]]) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], Int]]
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

    override def appendMetadata(md: Metadata): RequiredInt = ???
  }

}
