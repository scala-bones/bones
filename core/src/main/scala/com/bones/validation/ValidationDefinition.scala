package com.bones.validation

import java.net.URI
import java.text.Format
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.implicits._

import scala.math.Ordering.{BigDecimalOrdering, ByteOrdering, CharOrdering, DoubleOrdering, FloatOrdering, IntOrdering, LongOrdering, ShortOrdering}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object ValidationDefinition {

  /** Represents a validations operation */
  trait ValidationOp[T] {
    def isValid: T => Boolean
    def defaultError(t: T): String
    def description: String
  }

  /** The input is optional */
  case class OptionalValidation[T](op: ValidationOp[T])
      extends ValidationOp[Option[T]] {
    override def isValid: Option[T] => Boolean = {
      case None    => true
      case Some(i) => op.isValid(i)
    }

    override def defaultError(t: Option[T]): String =
      t.map(op.defaultError).getOrElse("N/A")

    override def description: String = op.description

  }

  /** Input must be one of the specified validValues. */
  case class ValidValue[T](validValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: T => Boolean = validValues.contains
    override def defaultError(t: T): String =
      s"DataClass $t must be one of ${validValues.mkString("('", "','", "')")}"
    override def description: String =
      s"one of ${validValues.mkString("('", "','", "')")}"
  }

  /** Input must not be one of the invalidValues. */
  case class InvalidValue[T](invalidValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: T => Boolean = str => { !invalidValues.contains(str) }
    override def defaultError(t: T): String =
      s"DataClass $t must not be one of ${invalidValues.mkString("('", "','", "')")}"
    override def description: String =
      s"not one of ${invalidValues.mkString("('", "','", "')")}"
  }

  /** Base trait for validation as to include valid and invalid values */
  trait BaseValidationOp[T] {
    def validVector(validValues: Vector[T]) = ValidValue(validValues)
    def valid(t: T*): ValidValue[T] = validVector(t.toVector)
    def invalidVector(invalidValues: Vector[T]) = InvalidValue(invalidValues)
    def invalid(t: T*): InvalidValue[T] = invalidVector(t.toVector)
  }


  object StringValidation extends BaseValidationOp[String] {

    val alphanumericRegexString = "^[a-zA-Z0-9]*$"
    val alphanumericRegex: Regex = alphanumericRegexString.r
    object IsAlphanumeric extends ValidationOp[String] {
      val isValid: String => Boolean =
        alphanumericRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not alphanumeric"

      override def description: String = s"alphanumeric"
    }

    val lettersWithSpaceRegexString = "^[a-zA-Z\\s]*$"
    val lettersWithSpaceRegex: Regex = lettersWithSpaceRegexString.r
    object Words extends ValidationOp[String] {

      val isValid: String => Boolean =
        lettersWithSpaceRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t contains characters other than letters and spaces"

      override def description: String = s"letters with space"
    }

    val sentenceRegexString = "^\\s+[A-Za-z,;'\"\\s]+[.?!]$"
    val sentenceRegex: Regex = sentenceRegexString.r
    object Sentence extends ValidationOp[String] {
      val isValid: String => Boolean =
        sentenceRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a sentence"

      override def description: String = s"sentence"
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

      override def defaultError(t: String): String =
        s"$t does not match regular expression ${r.toString}"

      override def description: String =
        s"must match regular expression ${r.toString}"
    }

    case class Length(length: Int) extends ValidationOp[String] {
      override def isValid: String => Boolean = _.length == length

      override def defaultError(t: String): String =
        s"$t does not have lengthO $length"

      override def description: String = s"lengthO of $length"
    }

    case class Custom(f: String => Boolean,
                      defaultErrorF: String => String,
                      description: String)
        extends ValidationOp[String] {
      val isValid: String => Boolean = f

      override def defaultError(t: String): String = defaultErrorF(t)
    }

    object Guid extends ValidationOp[String] {
      val isValid: String => Boolean = str =>
        Try {
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

    object Trimmed extends ValidationOp[String] {
      val isValid: String => Boolean = str => str.trim == str

      override def defaultError(t: String): String = s"'$t' must not have any leading or trailing whitespace"

      override def description: String = "trimmed"
    }

    object CreditCard extends ValidationOp[String] {
      override def isValid: String => Boolean =
        input => ValidationUtil.luhnCheck(10, input)

      override def defaultError(t: String): String =
        s"$t is not a valid credit card number"

      override def description: String = "valid credit card number"
    }

    val tokenRegex: Regex = "^[a-zA-Z0-9_]*$".r

    object Token extends ValidationOp[String] {
      override def isValid: String => Boolean =
        tokenRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a token"

      override def description: String = "token"
    }

    // From https://stackoverflow.com/questions/13912597/validate-email-one-liner-in-scala
    val emailRegex: Regex =
      """^[a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$""".r

    object Email extends ValidationOp[String] {
      override def isValid: String => Boolean =
        emailRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a valid email"

      override def description: String = "email"
    }

    val hexRegex: Regex = "^[0-9A-F]+$".r

    object Hex extends ValidationOp[String] {
      override def isValid: String => Boolean =
        hexRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not hexadecimal"

      override def description: String = "hexadecimal"
    }

    val base64Regex: Regex =
      "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$".r

    object Base64 extends ValidationOp[String] {
      override def isValid: String => Boolean =
        input => base64Regex.findFirstMatchIn(input.trim).isDefined

      override def defaultError(t: String): String = s"$t is not Base64"

      override def description: String = "Base64"
    }

    val hostnameRegex: Regex =
      "^(([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]*[a-zA-Z0-9])\\.)*([A-Za-z0-9]|[A-Za-z0-9][A-Za-z0-9\\-]*[A-Za-z0-9])$".r

    object Hostname extends ValidationOp[String] {
      override def isValid: String => Boolean =
        hostnameRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a hostname"

      override def description: String = "RFC1123 hostname"
    }

    val ipv4Regex: Regex =
      "^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$".r

    object Ipv4 extends ValidationOp[String] {
      override def isValid: String => Boolean =
        ipv4Regex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not an ipv4"

      override def description: String = "IPv4"
    }

    object Lowercase extends ValidationOp[String] {
      override def isValid: String => Boolean = str => str.toLowerCase === str

      override def defaultError(t: String): String = s"$t is not all lowercase"

      override def description: String = "lowercase"
    }

    object Uri extends ValidationOp[String] {
      override def isValid: String => Boolean =
        input =>
          Try {
            URI.create(input)
          }.isSuccess

      override def defaultError(t: String): String = s"$t is not a Uri"

      override def description: String = "URI"
    }

    val words: Words.type = Words

    val sentence: Sentence.type = Sentence

    /** Length of string must be equal to theLength param */
    def length(theLength: Int): Length = Length(theLength)

    /** Length must be greater than minLength param */
    def min(minLength: Int): MinLength = MinLength(minLength)

    /** Length must be less than maxLength param */
    def max(maxLength: Int): MaxLength = MaxLength(maxLength)

    /** String must match specified Regex */
    def matchesRegex(r: Regex): MatchesRegex = MatchesRegex(r)

    /** String must be alpha numeric */
    val alphanumeric: IsAlphanumeric.type = IsAlphanumeric

    /** String must be uppercase */
    val uppercase: Uppercase.type = Uppercase

    /** string must be trimmed (no leading or trailing whitespace). */
    val trimmed: Trimmed.type = Trimmed

    /** */
    def custom(f: String => Boolean,
               defaultErrorF: String => String,
               description: String) =
      Custom(f, defaultErrorF, description)

    /** String must be a guid */
    val guid: Guid.type = Guid

    /** String must be a valid email format */
    val email: Email.type = Email

    /** String must be a token, which is alpha numeric with underscore. */
    val token: Token.type = Token

    /** String must be a valid hexadecimal String */
    val hex: Hex.type = Hex

    /** String must be in base64 */
    val base64: Base64.type = Base64

    /** String must be a hostname */
    val hostname: Hostname.type = Hostname

    /** String must be an IPv4 */
    val iPv4: Ipv4.type = Ipv4

    /** String must be all lowercase, that is all letters in the string must be lowercase. */
    val lowercase: Lowercase.type = Lowercase

    /** String must be a Uri */
    val uri: Uri.type = Uri

    /** String must be a valid credit card number*/
    val creditCard: CreditCard.type = CreditCard
  }

  trait OrderingValidation[N] extends Ordering[N]{

    val zero: N

    case class Between(min: N, max: N) extends ValidationOp[N] {
      val isValid: N => Boolean = input => input >= min && input <= max

      override def defaultError(t: N): String =
        s"$t is not between $min and $max"

      override def description: String = s"between $min and $max"

    }

    case class Max(maxLong: N) extends ValidationOp[N] {
      override def isValid: N => Boolean = _ <= maxLong

      override def defaultError(t: N): String =
        s"$t is greater than $maxLong"

      override def description: String = s"maximum of $maxLong"
    }

    case class Min(minLong: N) extends ValidationOp[N] {
      override def isValid: N => Boolean = _ >= minLong

      override def defaultError(t: N): String = s"$t is less than $minLong"

      override def description: String = s"minimum of $minLong"
    }

    case class Greater(greaterThan: N) extends ValidationOp[N] {
      override def isValid: N => Boolean = _ > greaterThan

      override def defaultError(t: N): String =
        s"$t is not greater than $greaterThan"

      override def description: String = s"greater than $greaterThan"
    }

    case class Less(lessThan: N) extends ValidationOp[N] {
      override def isValid: N => Boolean = _ < lessThan

      override def defaultError(t: N): String =
        s"$t is not less than $lessThan"

      override def description: String = s"less than $lessThan"
    }

    object Positive extends ValidationOp[N] {
      override def isValid: N => Boolean = _ >= zero

      override def defaultError(t: N): String = s"$t is not positive"

      override def description: String = s"positive"
    }

    object Negative extends ValidationOp[N] {
      override def isValid: N => Boolean = _ <= zero

      override def defaultError(t: N): String = s"$t is not negative"

      override def description: String = s"negative"
    }

    def between(min: N, max: N): Between = Between(min, max)

    def max(maxValue: N): Max = Max(maxValue)

    def min(minValue: N): Min = Min(minValue)

    def greater(value: N): Greater = Greater(value)

    def less(value: N): Less = Less(value)

    def positive: Positive.type = Positive

    def negative: Negative.type = Negative

  }

  trait Modulo[N] extends OrderingValidation[N] {

    val modulo: (N,N) => N

    case class Multiple(multipleOf: N) extends ValidationOp[N] {
      override def isValid: N => Boolean = n => modulo(n,multipleOf) == zero

      override def defaultError(t: N): String =
        s"$t is not a multiple of $multipleOf"

      override def description: String = s"multiple of $multipleOf"
    }

    def multiple(n: N) = Multiple(n)
  }

  object CharValidation extends BaseValidationOp[Char] with OrderingValidation[Char] with CharOrdering {
    override val zero: Char = 0
  }

  object ByteValidation extends BaseValidationOp[Byte] with OrderingValidation[Byte] with Modulo[Byte] with ByteOrdering {
    override val zero: Byte = 0
    override val modulo: (Byte, Byte) => Byte = (b1, b2) => (b1 % b2).byteValue()
  }

  object ShortValidation extends BaseValidationOp[Short] with OrderingValidation[Short] with Modulo[Short] with ShortOrdering {
    override val modulo: (Short, Short) => Short = (i1, i2) => (i1 % i2).shortValue()
    val zero: Short = 0
  }

  object IntValidation extends BaseValidationOp[Int] with OrderingValidation[Int] with Modulo[Int] with IntOrdering {
    override val modulo: (Int, Int) => Int = (i1, i2) => i1 % i2
    val zero = 0
  }

  object LongValidation extends BaseValidationOp[Long] with OrderingValidation[Long] with Modulo[Long] with LongOrdering {
    override val modulo: (Long, Long) => Long = (l1,l2) => l1 % l2
    val zero = 0l
  }

  object DoubleValidation extends BaseValidationOp[Double] with OrderingValidation[Double] with DoubleOrdering {
    override val zero: Double = 0
  }

  object FloatValidation extends BaseValidationOp[Float] with OrderingValidation[Float] with FloatOrdering {
    override val zero: Float = 0
  }


  object BigDecimalValidation extends BaseValidationOp[BigDecimal] with OrderingValidation[BigDecimal] with BigDecimalOrdering {
    val zero = BigDecimal(0)
  }

  object DateValidationInstances {

    case class IsDate(format: Format, formatDescription: Option[String]) {
      def description: String =
        formatDescription.getOrElse(s"Is a Date with format ${format.toString}")
    }

    case class Min(minDate: LocalDateTime, format: DateTimeFormatter)
        extends ValidationOp[LocalDateTime] {
      override def isValid: LocalDateTime => Boolean = _.isAfter(minDate)

      override def defaultError(t: LocalDateTime): String =
        s"specified date ${format.format(t)} must be after ${format.format(minDate)}"

      override def description: String = s"after ${format.format(minDate)}"
    }

    case class Max(maxDate: LocalDateTime, format: DateTimeFormatter)
        extends ValidationOp[LocalDateTime] {
      override def isValid: LocalDateTime => Boolean = _.isBefore(maxDate)

      override def defaultError(t: LocalDateTime): String =
        s"specified date ${format.format(t)} must be before ${format.format(maxDate)}"

      override def description: String = s"before ${format.format(maxDate)}"
    }

  }

}
