package com.bones.validation

import java.net.URI
import java.text.Format
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.free.FreeApplicative
import cats.implicits._

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}



object ValidationDefinition {

  /** Represents a validations operation */
  trait ValidationOp[T] {
    def isValid: T => Boolean
    def defaultError(t: T): String
    def lift: Validation[T] = FreeApplicative.lift(this)
    def description: String
  }

  /** The input is optional */
  case class OptionalValidation[T](op: ValidationOp[T]) extends ValidationOp[Option[T]] {
    override def isValid: Option[T] => Boolean = {
      case None => true
      case Some(i) => op.isValid(i)
    }

    override def defaultError(t: Option[T]): String = t.map(op.defaultError).getOrElse("N/A")

    override def description: String = op.description

  }

  type Validation[A] = FreeApplicative[ValidationOp, A]

  /** Input must be one of the specified validValues. */
  case class ValidValue[T](validValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: T => Boolean = validValues.contains
    override def defaultError(t: T): String = s"DataClass $t must be one of ${validValues.mkString("('","','","')")}"
    override def description: String = s"one of ${validValues.mkString("('","','","')")}"
  }

  /** Input must not be one of the invalidValues. */
  case class InvalidValue[T](invalidValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: T => Boolean = str => {! invalidValues.contains(str)}
    override def defaultError(t: T): String = s"DataClass $t must not be one of ${invalidValues.mkString("('","','","')")}"
    override def description: String = s"not one of ${invalidValues.mkString("('","','","')")}"
  }

  /** Base trait for validation as to include valid and invalid values */
  trait BaseValidationOp[T] {
    def validVector(validValues: Vector[T]) = ValidValue(validValues)
    def valid(t: T*): ValidValue[T] = validVector(t.toVector)
    def invalidVector(invalidValues: Vector[T]) = InvalidValue(invalidValues)
    def invalid(t: T*): InvalidValue[T] = invalidVector(t.toVector)
  }

  private val alphanumRegx = "^[a-zA-Z0-9]*$".r

  object StringValidation extends BaseValidationOp[String] {

    object IsAlphanum extends ValidationOp[String] {
      val isValid: String => Boolean = alphanumRegx.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not alphanumeric"

      override def description: String = s"alphanumeric"
    }

    case class MinLength(min: Int) extends ValidationOp[String] {
      val isValid: String => Boolean = _.length >= min

      override def defaultError(t: String): String = s"$t is less than $min"

      override def description: String = s"minimum of $min"
    }

    case class MaxLength(max: Int) extends ValidationOp[String] {
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
      override def isValid: String => Boolean = _.length == length

      override def defaultError(t: String): String = s"$t does not have lengthO $length"

      override def description: String = s"lengthO of $length"
    }

    case class Custom(f: String => Boolean, defaultErrorF: String => String, description: String) extends ValidationOp[String] {
      val isValid: String => Boolean = f

      override def defaultError(t: String): String = defaultErrorF(t)
    }

    object Guid extends ValidationOp[String] {
      val isValid: String => Boolean = str => Try {
        UUID.fromString(str)
      } match {
        case Success(_) => true
        case Failure(_) => false
      }

      override def defaultError(t: String): String = s"$t is not a GUID"

      override def description: String = "be a GUID"
    }

    object Uppercase extends ValidationOp[String] {
      val isValid: String => Boolean = str => str.toUpperCase === str

      override def defaultError(t: String): String = s"$t must be uppercase"

      override def description: String = "uppercase"
    }

    object CreditCard extends ValidationOp[String] {
      override def isValid: String => Boolean = input => ValidationUtil.luhnCheck(10, input)

      override def defaultError(t: String): String = s"$t is not a valid credit card number"

      override def description: String = "valid credit card number"
    }

    private val tokenRegex = "^[a-zA-Z0-9_]*$".r

    object Token extends ValidationOp[String] {
      override def isValid: String => Boolean = tokenRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a token"

      override def description: String = "token"
    }

    // From https://stackoverflow.com/questions/13912597/validate-email-one-liner-in-scala
    val emailRegex: Regex =
      """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    object Email extends ValidationOp[String] {
      override def isValid: String => Boolean = emailRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a valid email"

      override def description: String = "email"
    }

    val hexRegex: Regex = "^[0-9A-F]+$".r

    object Hex extends ValidationOp[String] {
      override def isValid: String => Boolean = hexRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not hexadecimal"

      override def description: String = "hexadecimal"
    }

    val base64Regex: Regex = "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$".r

    object Base64 extends ValidationOp[String] {
      override def isValid: String => Boolean = input => base64Regex.findFirstMatchIn(input.trim).isDefined

      override def defaultError(t: String): String = s"$t is not Base64"

      override def description: String = "Base64"
    }

    val hostnameRegex: Regex = "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$".r

    object Hostname extends ValidationOp[String] {
      override def isValid: String => Boolean = hostnameRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a hostname"

      override def description: String = "RFC1123 hostname"
    }

    val ipv4Regex: Regex = "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$".r

    object Ipv4 extends ValidationOp[String] {
      override def isValid: String => Boolean = ipv4Regex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not an ipv4"

      override def description: String = "IPv4"
    }

    object Lowercase extends ValidationOp[String] {
      override def isValid: String => Boolean = str => str.toLowerCase === str

      override def defaultError(t: String): String = s"$t is not all lowercase"

      override def description: String = "lowercase"
    }

    object Uri extends ValidationOp[String] {
      override def isValid: String => Boolean = input => Try {
        URI.create(input)
      }.isSuccess

      override def defaultError(t: String): String = s"$t is not a Uri"

      override def description: String = "URI"
    }

    /** Length of string must be equal to theLength param */
    def length(theLength: Int): Length = Length(theLength)

    /** Length must be greater than minLength param */
    def min(minLength: Int): MinLength = MinLength(minLength)

    /** Length must be less than maxLength param */
    def max(maxLength: Int): MaxLength = MaxLength(maxLength)

    /** String must match specified Regex */
    def matchesRegex(r: Regex): MatchesRegex = MatchesRegex(r)

    /** String must be alpha numeric */
    def alphanum = IsAlphanum

    /** */
    def custom(f: String => Boolean, defaultErrorF: String => String, description: String) =
      Custom(f, defaultErrorF, description)

    /** String must be a guid */
    def guid = Guid

    /** String must be a valid email format */
    def email = Email

    /** String must be a token, which is alpha numeric with underscore. */
    def token = Token

    /** String must be a valid hexadecimal String */
    def hex = Hex

    /** String must be in base64 */
    def base64 = Base64

    /** String must be a hostname */
    def hostname = Hostname

    /** String must be an IPv4 */
    def iPv4 = Ipv4

    /** String must be all lowercase, that is all letters in the string must be lowercase. */
    def lowercase = Lowercase

    /** String must be a Uri */
    def uri = Uri

    /** String must be a valid credit card */
    def creditCard = CreditCard
  }


  object LongValidation extends BaseValidationOp[Long] {

    case class Between(min: Long, max: Long) extends ValidationOp[Long] {
      val isValid: Long => Boolean = input => input >= min && input <= max

      override def defaultError(t: Long): String = s"$t is not between $min and $max"

      override def description: String = s"between $min and $max"

    }

    case class Max(maxLong: Long) extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ <= maxLong

      override def defaultError(t: Long): String = s"$t is greater than $maxLong"

      override def description: String = s"maximum of $maxLong"
    }

    case class Min(minLong: Long) extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ >= minLong

      override def defaultError(t: Long): String = s"$t is less than $minLong"

      override def description: String = s"minimum of $minLong"
    }

    case class Greater(greaterThan: Long) extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ > greaterThan

      override def defaultError(t: Long): String = s"$t is not greater than $greaterThan"

      override def description: String = s"greater than $greaterThan"
    }

    case class Less(lessThan: Long) extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ < lessThan

      override def defaultError(t: Long): String = s"$t is not less than $lessThan"

      override def description: String = s"less than $lessThan"
    }

    case class Multiple(multipleOf: Long) extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ % multipleOf === 0

      override def defaultError(t: Long): String = s"$t is not a multiple of $multipleOf"

      override def description: String = s"multiple of $multipleOf"
    }

    object Positive extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ >= 0

      override def defaultError(t: Long): String = s"$t is not positive"

      override def description: String = s"positive"
    }

    object Negative extends ValidationOp[Long] {
      override def isValid: Long => Boolean = _ <= 0

      override def defaultError(t: Long): String = s"$t is not negative"

      override def description: String = s"negative"
    }


    def between(min: Long, max: Long): Between = Between(min, max)

    def max(maxValue: Long): Max = Max(maxValue)

    def min(minValue: Long): Min = Min(minValue)

    def greater(value: Long): Greater = Greater(value)

    def less(value: Long): Less = Less(value)

    def positive = Positive

    def negative = Negative
  }

  object BigDecimalValidation {

    case class Max(max: BigDecimal) extends ValidationOp[BigDecimal] {
      override def isValid: BigDecimal => Boolean = inputBd => max >= inputBd

      override def defaultError(t: BigDecimal): String = s"$t is greater than the maximum $max"

      override def description: String = s"maximum value of ${max.toString()}"
    }

    case class Min(min: BigDecimal) extends ValidationOp[BigDecimal] {
      override def isValid: BigDecimal => Boolean = inputBd => min <= inputBd

      override def defaultError(t: BigDecimal): String = s"$t is less than the minimum $min"

      override def description: String = s"minimum value of $min"
    }

    case object Positive extends ValidationOp[BigDecimal] {
      override def isValid: BigDecimal => Boolean = _ > BigDecimal(0)

      override def defaultError(t: BigDecimal): String = s"$t must be positive"

      override def description: String = s"positive"
    }

    def min(m: BigDecimal) = Min(m)
    def max(m: BigDecimal) = Max(m)
    val positive = Positive

  }

  object DateValidationInstances {

    case class IsDate(format: Format, formatDescription: Option[String]) {
      def description: String = formatDescription.getOrElse(s"Is a Date with format ${format.toString}")
    }

    case class Min(minDate: ZonedDateTime, format: DateTimeFormatter) extends ValidationOp[ZonedDateTime] {
      override def isValid: ZonedDateTime => Boolean = _.isAfter(minDate)

      override def defaultError(t: ZonedDateTime): String = s"specified date ${format.format(t)} must be after ${format.format(minDate)}"

      override def description: String = s"after ${format.format(minDate)}"
    }

    case class Max(maxDate: ZonedDateTime, format: DateTimeFormatter) extends ValidationOp[ZonedDateTime] {
      override def isValid: ZonedDateTime => Boolean = _.isBefore(maxDate)

      override def defaultError(t: ZonedDateTime): String = s"specified date ${format.format(t)} must be before ${format.format(maxDate)}"

      override def description: String = s"before ${format.format(maxDate)}"
    }


  }

}
