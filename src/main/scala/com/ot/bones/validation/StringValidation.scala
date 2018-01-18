package com.ot.bones.validation

import java.net.URI
import java.util.UUID

import cats.Id
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._
import com.ot.bones.interpreter.ExtractionInterpreter.{AppendGenericValidation, ExtractionErrors, RequiredOp, StringProducer, ValidationError, ValidationOp, ValidationResultNel}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object StringValidation {

  private val alphanumRegx = "^[a-zA-Z0-9]*$".r

  case class IsAlphanum() extends ValidationOp[String] {
    val isValid: String => Boolean = alphanumRegx.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not alphanumeric"

    override def description: String = s"alphanumeric"
  }

  case class Min(min: Int) extends ValidationOp[String] {
    val isValid: String => Boolean = _.length >= min

    override def defaultError(t: String): String = s"$t is less than $min"

    override def description: String = s"minimum of $min"
  }

  case class Max(max: Int) extends ValidationOp[String] {
    val isValid: String => Boolean = _.length <= max

    override def defaultError(t: String): String = s"$t is greater than $max"

    override def description: String = s"maximum of $max"
  }

  case class MatchesRegex(r: Regex) extends ValidationOp[String] {
    val isValid: String => Boolean = r.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t does not match regular expression ${r.toString}"

    override def description: String = s"must match regular expression ${r.toString}"
  }

  case class Length(length: Int) extends ValidationOp[String] {
    override def isValid: (String) => Boolean = _.length == length

    override def defaultError(t: String): String = s"$t does not have length $length"

    override def description: String = s"length of $length"
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

    override def defaultError(t: String): String = s"$t is not a GUID"

    override def description: String = "be a GUID"
  }

  case class Uppercase() extends ValidationOp[String] {
    val isValid: String => Boolean = str => str.toUpperCase === str

    override def defaultError(t: String): String = s"$t must be uppercase"

    override def description: String = "uppercase"
  }

  def digitToInt(x:Char) = x.toInt - '0'.toInt

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


  case class CreditCard() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = input => luhnCheck(10, input)
    override def defaultError(t: String): String = s"$t is not a valid credit card number"

    override def description: String = "valid credit card number"
  }

  private val tokenRegex = "^[a-zA-Z0-9_]*$".r

  case class Token() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = tokenRegex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not a token"

    override def description: String = "token"
  }

  // From https://stackoverflow.com/questions/13912597/validate-email-one-liner-in-scala
  val emailRegex: Regex = """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

  case class Email() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = emailRegex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not a valid email"

    override def description: String = "email"
  }

  val hexRegex: Regex = "^[0-9A-F]+$".r
  case class Hex() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = hexRegex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not hexadecimal"

    override def description: String = "hexadecimal"
  }

  val base64Regex: Regex = "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$".r
  case class Base64() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = input => base64Regex.findFirstMatchIn(input.trim).isDefined

    override def defaultError(t: String): String = s"$t is not Base64"

    override def description: String = "Base64"
  }

  val hostnameRegex: Regex = "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\-]*[A-Za-z0-9])$".r
  case class Hostname() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = hostnameRegex.findFirstMatchIn(_).isDefined
    override def defaultError(t: String): String = s"$t is not a hostname"

    override def description: String = "RFC1123 hostname"
  }

  val ipv4Regex: Regex = "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$".r
  case class Ipv4() extends ValidationOp[String] {
    override def isValid: String => Boolean = ipv4Regex.findFirstMatchIn(_).isDefined

    override def defaultError(t: String): String = s"$t is not an ipv4"

    override def description: String = "IPv4"
  }

  case class Lowercase() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = str => str.toLowerCase === str

    override def defaultError(t: String): String = s"$t is not all lowercase"

    override def description: String = "lowercase"
  }

  case class Uri() extends ValidationOp[String] {
    override def isValid: String => Boolean = input => Try { URI.create(input) }.isSuccess

    override def defaultError(t: String): String = s"$t is not a Uri"

    override def description: String = "URI"
  }


  /**
    * This is for the syntactic sugar of adding validation types.
    *
    * @tparam NSE The 'Next StringExtraction' type if a new value is appended.
    */
  trait StringExtraction[F[_], NSE] extends AppendGenericValidation[String, F, NSE] {

    /** Length of string must be equal to theLength param */
    def length(theLength: Int): NSE = append(Length(theLength))

    /** Length must be greater than minLength param */
    def min(minLength: Int): NSE = append(Min(minLength))

    /** Length must be less than maxLength param */
    def max(maxLength: Int): NSE = append(Max(maxLength))

    /** String must match specified Regex */
    def matchesRegex(r: Regex): NSE = append(MatchesRegex(r))

    /** String must be alpha numeric */
    def alphanum(): NSE = append(IsAlphanum())

    /** String must be a guid */
    def guid(): NSE = append(Guid())

    /** String must be a valid email format */
    def email(): NSE = append(Email())

    /** String must be a token, which is alpha numeric with underscore. */
    def token(): NSE = append(Token())

    /** String must be a valid hexadecimal String */
    def hex(): NSE = append(Hex())

    /** String must be in base64 */
    def base64(): NSE = append(Base64())

    /** String must be a hostname */
    def hostname(): NSE = append(Hostname())

    /** String must be an IPv4 */
    def iPv4(): NSE = append(Ipv4())

    /** String must be all lowercase, that is all letters in the string must be lowercase. */
    def lowercase(): NSE = append(Lowercase())

    /** String must be a Uri */
    def uri(): NSE = append(Uri())


  }

  /**
    * FieldGroup Operation declares that a key is an optional string and passes the specified list of validation.
    * @param key The key used to extract a value.
    * @param validations List of validations that the String must pass.
    */

  final case class OptionalString(key: Key, validations: List[ValidationOp[String]]) extends DataDefinitionOp[Option[String]]
    with StringExtraction[Option, OptionalString] {

    def extract(input: StringProducer): Validated[ExtractionErrors, Option[String]] = {
      input.produceString(key).toValidatedNel.andThen {
        case Some(str) =>
          ValidationUtil.runAndMapValidations(key, str, validations).map(Some(_))
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

    def extract(producer: StringProducer): ValidationResultNel[String] = {
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



