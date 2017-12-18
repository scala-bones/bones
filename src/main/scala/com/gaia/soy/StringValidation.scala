package com.gaia.soy

import java.util.UUID

import cats.{Apply, Id}
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

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


  private def runValidation(key: Key, input: String, stringValidation: ValidationOp[String]): Validated[ValidationError[String], String] = {
    if (stringValidation.isValid(input)) {
      Valid(input)
    } else {
      Invalid(ValidationError(key, stringValidation, Some(input)))
    }
  }


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

  private def runAndMapValidations(key: Key, str: String, validations: List[ValidationOp[String]]): ValidationResultNel[String] =
    validations.map(runValidation(key, str, _))
      .foldLeft[ValidatedNel[ValidationError[String], String]](Valid(str))((last, next) => {
        val n: ValidatedNel[ValidationError[String], String] = next.leftMap(NonEmptyList.one)
        Apply[ValidatedNel[ValidationError[String], ?]].map2(last, n)((a, b) => b)
      })


  final case class OptionalString(key: Key, validations: List[ValidationOp[String]]) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], Option[String]]]
    with StringExtraction[Option, OptionalString] {

    def extract(input: JsonProducer): Validated[ExtractionErrors, Option[String]] = {
      input.produceString(key).toValidatedNel.andThen {
        case Some(str) => {
          runAndMapValidations(key, str, validations).map(Some(_))
        }
        case None => Valid(None)
      }
    }


    override def appendMetadata(md: Metadata): OptionalString = ???

    def append(sv: ValidationOp[String]): OptionalString = OptionalString(key, sv :: validations)
  }

  final case class RequiredString(key: Key, validations: List[ValidationOp[String]]) extends FieldGroupOp[Validated[NonEmptyList[ExtractionError], String]]
    with StringExtraction[Id, RequiredString] {

    def optional(): OptionalString = OptionalString(key, validations)

    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], String] = {
      producer.produceString(key).toValidated.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(ValidationError(key, RequiredOp(), None)))
      }.andThen(str =>
        runAndMapValidations(key, str, validations)
      )
    }

    def append(sv: ValidationOp[String]): RequiredString = RequiredString(key, sv :: validations)

    override def appendMetadata(md: Metadata): RequiredString = ???
  }

  /** ****** Conversion *************/

  //  trait StringConversion[O] {
  //    def conversionOp: ConversionOp[String,O]
  //  }
  //
  //  case class ToBigDecimal() extends ConversionOp[String, BigDecimal] {
  //    def convert(str: String) : Either[String, BigDecimal] =
  //      Try { BigDecimal(str) }.toEither.left.map(_ => "Could not convert String to BigDecimal")
  //  }
  //
  //  case class ToUuid() extends ConversionOp[String, UUID] {
  //    override def convert(iToO: String): Either[String, UUID] =
  //      Try { UUID.fromString(iToO) }.toEither.left.map(_ => "Could not convert String to UUID")
  //  }

}



