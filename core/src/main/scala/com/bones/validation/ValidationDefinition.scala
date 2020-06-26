package com.bones.validation

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}

import cats.implicits._

import scala.math.Ordering.{
  BigDecimalOrdering,
  ByteOrdering,
  CharOrdering,
  IntOrdering,
  LongOrdering,
  ShortOrdering
}
import scala.math.Ordering.Float.{TotalOrdering => FloatOrdering}
import scala.math.Ordering.Double.{TotalOrdering => DoubleOrdering }
import scala.util.matching.Regex

/**
  * A collection of validation definition natively supported by Bones.
  */
object ValidationDefinition {

  /** Represents a validations operation */
  trait ValidationOp[T] {

    /** Returns true if T passes the validation */
    def isValid: T => Boolean

    /** If t is not valid, this will return the error message in English. */
    def defaultError(t: T): String

    /** Gives an English text description of the validation. */
    def description: String
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
    override def isValid: T => Boolean = str => {
      !invalidValues.contains(str)
    }

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

  case class EnumerationValidation[E <: Enumeration]() extends BaseValidationOp[E]

  /**
    * A collection of ValidationOp[String] objects.
    */
  object StringValidation extends BaseValidationOp[String] {

    val alphanumericRegexString = "^[a-zA-Z0-9]*$"
    val alphanumericRegex: Regex = alphanumericRegexString.r

    /** Uses regex to determine if the string is Alphanumeric */
    object IsAlphanumeric extends ValidationOp[String] {
      val isValid: String => Boolean =
        alphanumericRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not alphanumeric"

      override def description: String = s"alphanumeric"
    }

    val wordsWithSpaceRegexString = "^[a-zA-Z\\s]*$"
    val wordsWithSpaceRegex: Regex = wordsWithSpaceRegexString.r

    /** Uses regex to determine if the string only contains words and whitespace */
    object Words extends ValidationOp[String] {

      val isValid: String => Boolean =
        wordsWithSpaceRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String =
        s"$t contains characters other than letters and spaces"

      override def description: String = s"letters with space"
    }

    val sentenceRegexString = "^[A-Za-z,;'\"\\s]+[.?!]$"
    val sentenceRegex: Regex = sentenceRegexString.r

    object Sentence extends ValidationOp[String] {
      val isValid: String => Boolean =
        sentenceRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a sentence"

      override def description: String = s"sentence"
    }

    /**
      * Defines min as the minimum length the string can be.
      *
      * @param min The minimum length the string can be.
      */
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

    /** A Custom validation for the string type.
      * Follow the values defined in [[ValidationOp]]
      */
    case class Custom(f: String => Boolean, defaultErrorF: String => String, description: String)
        extends ValidationOp[String] {
      val isValid: String => Boolean = f

      override def defaultError(t: String): String = defaultErrorF(t)
    }

    object Uppercase extends ValidationOp[String] {
      val isValid: String => Boolean = str => str.toUpperCase === str

      override def defaultError(t: String): String = s"$t must be uppercase"

      override def description: String = "uppercase"
    }

    object Trimmed extends ValidationOp[String] {
      val isValid: String => Boolean = str => str.trim == str

      override def defaultError(t: String): String =
        s"'$t' must not have any leading or trailing whitespace"

      override def description: String = "trimmed"
    }

    val tokenRegex: Regex = "^[a-zA-Z0-9_]*$".r

    object Token extends ValidationOp[String] {
      override def isValid: String => Boolean =
        tokenRegex.findFirstMatchIn(_).isDefined

      override def defaultError(t: String): String = s"$t is not a token"

      override def description: String = "token"
    }

    object Lowercase extends ValidationOp[String] {
      override def isValid: String => Boolean = str => str.toLowerCase === str

      override def defaultError(t: String): String = s"$t is not all lowercase"

      override def description: String = "lowercase"
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

    /** String must be all lowercase, that is all letters in the string must be lowercase. */
    val lowercase: Lowercase.type = Lowercase

    /** */
    def custom(f: String => Boolean, defaultErrorF: String => String, description: String) =
      Custom(f, defaultErrorF, description)

    /** String must be a token, which is alpha numeric with underscore. */
    val token: Token.type = Token

  }

  trait ZeroValidations[N] extends Ordering[N] {

    /** What is the zero Value of N.  Used for positive and negative comparisons */
    val zero: N

    object Positive extends ValidationOp[N] {
      override def isValid: N => Boolean = _ > zero

      override def defaultError(t: N): String = s"$t is not positive"

      override def description: String = s"positive"
    }

    object Negative extends ValidationOp[N] {
      override def isValid: N => Boolean = _ < zero

      override def defaultError(t: N): String = s"$t is not negative"

      override def description: String = s"negative"
    }

    /** Ensure an instance of type N is positive */
    def positive: Positive.type = Positive

    /** Ensure an instance of type N is negative */
    def negative: Negative.type = Negative

  }

  /**
    * Base trait for any type which has an ordering.  Must be extended with specific type.
    **/
  trait OrderingValidation[N] extends Ordering[N] {

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

    /** Ensure an instance of type N is inclusively between min and max */
    def between(min: N, max: N): Between = Between(min, max)

    /** Ensure the max value of an instance of type N is maxValue */
    def max(maxValue: N): Max = Max(maxValue)

    /** Ensure the minimum value of an instance of type N is minValue */
    def min(minValue: N): Min = Min(minValue)

    /** Ensure an instance of type N is greater than (exclusive) value */
    def greater(value: N): Greater = Greater(value)

    /** Ensure an instance of type N is less than (exclusive) value */
    def less(value: N): Less = Less(value)

  }

  /** If we can define modulo for the type N and we can use ZeroValidations,
    * then we can calculate multiples of type N */
  trait Modulo[N] extends ZeroValidations[N] {

    val modulo: (N, N) => N

    case class Multiple(multipleOf: N) extends ValidationOp[N] {
      override def isValid: N => Boolean = n => modulo(n, multipleOf) == zero

      override def defaultError(t: N): String =
        s"$t is not a multiple of $multipleOf"

      override def description: String = s"multiple of $multipleOf"
    }

    def multiple(n: N) = Multiple(n)
  }

  object CharValidation
      extends BaseValidationOp[Char]
      with OrderingValidation[Char]
      with ZeroValidations[Char]
      with CharOrdering {
    override val zero: Char = 0
  }

  object ByteValidation
      extends BaseValidationOp[Byte]
      with OrderingValidation[Byte]
      with Modulo[Byte]
      with ByteOrdering {
    override val zero: Byte = 0
    override val modulo: (Byte, Byte) => Byte = (b1, b2) => (b1 % b2).byteValue()
  }

  object ShortValidation
      extends BaseValidationOp[Short]
      with OrderingValidation[Short]
      with Modulo[Short]
      with ShortOrdering {
    override val modulo: (Short, Short) => Short = (i1, i2) => (i1 % i2).shortValue()
    val zero: Short = 0
  }

  object IntValidation
      extends BaseValidationOp[Int]
      with OrderingValidation[Int]
      with Modulo[Int]
      with IntOrdering {
    override val modulo: (Int, Int) => Int = (i1, i2) => i1 % i2
    val zero = 0

    case class InRanges(ranges: List[Range])

    def inRanges(range: Range*) = InRanges(range.toList)

  }

  object LongValidation
      extends BaseValidationOp[Long]
      with OrderingValidation[Long]
      with Modulo[Long]
      with LongOrdering {
    override val modulo: (Long, Long) => Long = (l1, l2) => l1 % l2
    val zero = 0l
  }

  object DoubleValidation
      extends BaseValidationOp[Double]
      with OrderingValidation[Double]
      with ZeroValidations[Double]
      with DoubleOrdering {
    override val zero: Double = 0
  }

  object FloatValidation
      extends BaseValidationOp[Float]
      with OrderingValidation[Float]
      with ZeroValidations[Float]
      with FloatOrdering {
    override val zero: Float = 0
  }

  object BigDecimalValidation
      extends BaseValidationOp[BigDecimal]
      with OrderingValidation[BigDecimal]
      with ZeroValidations[BigDecimal]
      with BigDecimalOrdering {
    val zero = BigDecimal(0)
  }

  trait BaseDateValidation[A] extends BaseValidationOp[A] with Ordering[A] {

    def defaultFormatToString(f: A): String

    /** Used in the error string to describe the type.  For instance: 'date' */
    val instantDescription: String

    case class MinTime(
      minDate: A,
      formatToString: A => String,
      instantDescription: String,
      ordering: Ordering[A])
        extends ValidationOp[A] {
      override def isValid: A => Boolean = (a: A) => ordering.compare(minDate, a) <= 0

      override def defaultError(t: A): String =
        s"specified ${instantDescription} ${formatToString(t)} must be after ${formatToString(minDate)}"

      override def description: String = s"after ${formatToString(minDate)}"
    }

    case class MaxTime(
      maxDate: A,
      formatToString: A => String,
      instantDescription: String,
      ordering: Ordering[A])
        extends ValidationOp[A] {
      override def isValid: A => Boolean = a => ordering.compare(a, maxDate) <= 0

      override def defaultError(t: A): String =
        s"specified ${instantDescription} ${formatToString(t)} must be before ${formatToString(maxDate)}"

      override def description: String = s"before ${formatToString(maxDate)}"
    }

    def min(minDate: A, format: A => String): MinTime =
      MinTime(minDate, format(_), instantDescription, this)

    def min(minDate: A): MinTime = MinTime(minDate, defaultFormatToString, instantDescription, this)

    def max(maxDate: A, format: A => String) = MaxTime(maxDate, format(_), instantDescription, this)

    def max(maxDate: A): MaxTime = MaxTime(maxDate, defaultFormatToString, instantDescription, this)

  }


}
