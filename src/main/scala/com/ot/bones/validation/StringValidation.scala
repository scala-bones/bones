package com.ot.bones.validation

import java.util.UUID

import cats.Id
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._
import com.ot.bones.interpreter.ExtractionInterpreter.{ExtractionAppend, ExtractionErrors, JsonProducer, RequiredOp, ValidationError, ValidationOp, ValidationResultNel}

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object StringValidation {

  private val alphanumRegx = "^[a-zA-Z0-9]*$".r

  case class IsAlphanum() extends ValidationOp[String] {
    val isValid: String => Boolean = alphanumRegx.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"${t} is not alphanumeric"

    override def description: String = s"alphanumeric"
  }

  case class Min(min: Int) extends ValidationOp[String] {
    val isValid: String => Boolean = _.length >= min

    override def defaultError(t: String): String = s"${t} is less than ${min}"

    override def description: String = s"minimum of ${min}"
  }

  case class Max(max: Int) extends ValidationOp[String] {
    val isValid: String => Boolean = _.length <= max

    override def defaultError(t: String): String = s"${t} is greater than ${max}"

    override def description: String = s"maximum of ${max}"
  }

  case class MatchesRegex(r: Regex) extends ValidationOp[String] {
    val isValid: String => Boolean = r.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"${t} does not match regular expression ${r.toString}"

    override def description: String = s"must match regular expression ${r.toString}"
  }

  case class Length(length: Int) extends ValidationOp[String] {
    override def isValid: (String) => Boolean = _.length == length

    override def defaultError(t: String): String = s"${t} does not have length ${length}"

    override def description: String = s"length of ${length}"
  }

  case class Custom(f: String => Boolean, defaultErrorF: String => String, description: String) extends ValidationOp[String] {
    val isValid: String => Boolean = f

    override def defaultError(t: String): String = defaultErrorF(t)
  }

  case class Guid() extends ValidationOp[String] {
    val isValid: String => Boolean = str => Try {
      UUID.fromString(str)
    } match {
      case Success(_) => true
      case Failure(_) => false
    }

    override def defaultError(t: String): String = s"${t} is not a GUID"

    override def description: String = "be a GUID"
  }

  case class Uppercase() extends ValidationOp[String] {
    val isValid: String => Boolean = str => str.toUpperCase === str

    override def defaultError(t: String): String = s"${t} must be uppercase"

    override def description: String = "uppercase"
  }

  case class CreditCard() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ??? //luhn check
    override def defaultError(t: String): String = s"${t} is not a valid credit card number"

    override def description: String = "valid credit card number"
  }

  private val tokenRegex = "^[a-zA-Z0-9_]*$".r

  case class Token() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = tokenRegex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"${t} is not a token"

    override def description: String = "token"
  }

  case class Email() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ???

    override def defaultError(t: String): String = s"${t} is not a valid email"

    override def description: String = "email"
  }

  case class Hex() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ???

    override def defaultError(t: String): String = s"${t} is not hexadecimal"

    override def description: String = "hexadecimal"
  }

  case class Base64() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ???

    override def defaultError(t: String): String = s"${t} is not Base64"

    override def description: String = "Base64"
  }

  case class Hostname() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ??? //RFC1123 hostname
    override def defaultError(t: String): String = s"${t} is not a hostname"

    override def description: String = "RFC1123 hostname"
  }

  case class Lowercase() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = str => str.toLowerCase === str

    override def defaultError(t: String): String = s"${t} is not all lowercase"

    override def description: String = "lowercase"
  }

  case class Ip() //enum ip. ipv4, ipv6, cidr
  case class Uri() //


  /**
    * This is for the syntactic sugar of adding validation types.
    *
    * @tparam NSE The 'Next StringExtraction' type if a new value is appended.
    */
  trait StringExtraction[F[_], NSE] extends ExtractionAppend[String, F, NSE] {

    def length(size: Int): NSE = append(Length(size))

    def min(min: Int): NSE = append(Min(min))

    def max(max: Int): NSE = append(Max(max))

    def matchesRegex(r: Regex): NSE = append(MatchesRegex(r))

    def alphanum(): NSE = append(IsAlphanum())

    def guid(): NSE = append(Guid())

    def email(): NSE = append(Email())

    def token(): NSE = append(Token())

    def hex(): NSE = append(Hex())

    def base64(): NSE = append(Base64())

    def replace(): NSE = ??? //convert ???
    def hostname(): NSE = ???

    def normalize(): NSE = ??? //convert ???
    def lowercase(): NSE = append(Lowercase())


  }

  /**
    * FieldGroup Operation declares that a key is an optional string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */

  final case class OptionalString(key: Key, validations: List[ValidationOp[String]]) extends DataDefinitionOp[Option[String]]
    with StringExtraction[Option, OptionalString] {

    def extract(input: JsonProducer): Validated[ExtractionErrors, Option[String]] = {
      input.produceString(key).toValidatedNel.andThen {
        case Some(str) => {
          ValidationUtil.runAndMapValidations(key, str, validations).map(Some(_))
        }
        case None => Valid(None)
      }
    }


    def append(sv: ValidationOp[String]): OptionalString = OptionalString(key, sv :: validations)
  }

  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */
  final case class RequiredString(key: Key, validations: List[ValidationOp[String]]) extends DataDefinitionOp[String]
    with StringExtraction[Id, RequiredString] {

    def optional(): OptionalString = OptionalString(key, validations)

    def extract(producer: JsonProducer): ValidationResultNel[String] = {
      producer.produceString(key).toValidated.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(ValidationError(key, RequiredOp(), None)))
      }.andThen(str =>
        ValidationUtil.runAndMapValidations(key, str, validations)
      )
    }

    def append(sv: ValidationOp[String]): RequiredString = RequiredString(key, sv :: validations)

  }

}



