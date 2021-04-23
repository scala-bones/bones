package com.bones

import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.time.{LocalDate, LocalTime}
import java.util.UUID
import com.bones.data.Error.{CanNotConvert, ExtractionError, ExtractionErrors}
import com.bones.data.HigherOrderValue
import shapeless.{HList, UnaryTCConstraint}

import scala.util.Try

/**
  * A collection of helper functions used by Bones.
  */
object Util {

  /** A Value that is defined in the schema, but in certain context, can be omitted
    * from the data. */
  case class OmittedValue[K](fieldName: String, typeName: String, path: Path[K])

  /** Either: Left means a value has been omitted, Right means the value has been specified */
  type CanBeOmitted[K, T] = Either[List[OmittedValue[K]], T]

  def allCharsAreDigits(str: String, isNegative: Boolean): Boolean =
    if (isNegative) str.drop(1).forall(Character.isDigit)
    else str.forall(Character.isDigit)

  def camelToWords(camel: String): String = {
    camel
      .split("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])")
      .map(_.toLowerCase.capitalize)
      .mkString(" ")
  }

  val maxLongLength = 19 // "9223372036854775807".size
  /** Convert to long trying to avoid stack dump for most common cases */
  def stringToLong(str: String): Option[Long] = {
    val isNegative = str.charAt(0) == '-'
    def isValidLength =
      if (isNegative) str.length < maxLongLength
      else str.length < maxLongLength + 1

    if (!str.isEmpty && isValidLength && allCharsAreDigits(str, isNegative)) {
      Try(str.toLong).toOption
    } else {
      None
    }
  }

  val maxShortLength = 5

  /** Convert to long trying to avoid stack dump for most common cases */
  def stringToShort(str: String): Option[Short] = {
    val isNegative = str.charAt(0) == '-'
    def isValidLength =
      if (isNegative) str.length < maxShortLength
      else str.length < maxShortLength + 1

    if (!str.isEmpty && isValidLength && allCharsAreDigits(str, isNegative)) {
      Try(str.toShort).toOption
    } else {
      None
    }
  }

  /** Convert a String to a UUID returning Left[NoneEmptyList[ExtractionError],UUID]
    * if there is a failure in conversion */
  def stringToUuid[K](uuidString: String, path: List[K]): Either[ExtractionErrors[K], UUID] =
    try {
      Right(UUID.fromString(uuidString))
    } catch {
      case e: IllegalArgumentException =>
        Left(List(CanNotConvert(path, uuidString, classOf[UUID], Some(e))))
    }

  /**
    * Convert the String to a LocalDate returning Either[ExtractionErrors[K],LocalDate]
    */
  def stringToLocalDate[K](
    input: String,
    dateFormat: DateTimeFormatter,
    path: List[K]): Either[ExtractionErrors[K], LocalDate] =
    try {
      Right(LocalDate.parse(input, dateFormat))
    } catch {
      case e: DateTimeParseException =>
        Left(List(CanNotConvert(path, input, classOf[LocalDate], Some(e))))
    }

  def stringToLocalTime[K](
    input: String,
    dateFormat: DateTimeFormatter,
    path: List[K]): Either[ExtractionErrors[K], LocalTime] =
    try {
      Right(LocalTime.parse(input, dateFormat))
    } catch {
      case e: DateTimeParseException =>
        Left(List(CanNotConvert(path, input, classOf[LocalDate], Some(e))))
    }

  /**
    * Convert the String to a BigDecimal returning Either[ExtractionErrors[K],BigDecimal]
    */
  def stringToBigDecimal[K](input: String, path: List[K]): Either[ExtractionErrors[K], BigDecimal] =
    try {
      Right(BigDecimal(input))
    } catch {
      case e: NumberFormatException =>
        Left(List(CanNotConvert(path, input, classOf[BigDecimal], Some(e))))
    }

  /** Convert the String to an Enumeration using scala.Enumeration.withName returning Left[ExtractionErrors[K],Object]
    * if there is an parse error.
    */
  def stringToEnumeration[K, E <: Enumeration, V](
    str: String,
    path: List[K],
    enumeration: E): Either[List[CanNotConvert[K, String, V]], enumeration.Value] =
    try {
      Right(enumeration.withName(str))
    } catch {
      case e: NoSuchElementException =>
        Left(List(CanNotConvert(path, str, manifest.runtimeClass.asInstanceOf[Class[V]], Some(e))))
    }

  /** Convert the string to an Enum using scala.Enumeration.withName returning Either[ExtractionErrors[K],A]
    * if there is an parse error.
    */
  def stringToEnum[K, A <: Enum[A]: Manifest](
    str: String,
    path: List[K],
    enums: List[A]): Either[List[CanNotConvert[K, String, A]], A] = {
    val manifestA = manifest[A]
    enums
      .find(_.toString == str)
      .toRight(
        List(
          CanNotConvert[K, String, A](
            path,
            str,
            manifestA.runtimeClass.asInstanceOf[Class[A]],
            None)))
  }

  /** Converts a long to a LocalDate.  Never fails */
  def longToLocalDate[K]: (Long, List[String]) => Either[ExtractionErrors[K], LocalDate] =
    (l, p) => Right(LocalDate.ofEpochDay(l))

  def longToLocalTime[K]: (Long, List[String]) => Either[ExtractionErrors[K], LocalTime] =
    (l, p) => Right(LocalTime.ofNanoOfDay(l))

  def sequence[B, E](input: List[Either[List[E], B]]): Either[List[E], List[B]] = {
    input match {
      case Nil => Right(List.empty)
      case x :: xs => {
        val tail = sequence(xs)
        (x, tail) match {
          case (Left(errs1), Left(errs2)) => Left(errs1 ::: errs2)
          case (Left(errs), _)            => Left(errs)
          case (_, Left(errs))            => Left(errs)
          case (Right(b1), Right(b2))     => Right(b1 :: b2)
        }
      }
    }
  }

  /**
    * Accumulates error on the left or combines success on the right, just
    * like Applicative.map2 if Either was a Validation.
    */
  def eitherMap2[K, A, B, Z, E](e1: Either[List[E], A], e2: Either[List[E], B])(
    f: (A, B) => Z): Either[List[E], Z] = {
    (e1, e2) match {
      case (Left(err1), Left(err2)) => Left(err1 ::: err2)
      case (Left(err1), _)          => Left(err1)
      case (_, Left(err2))          => Left(err2)
      case (Right(a), Right(b))     => Right(f(a, b))
    }
  }

  def eitherMap2HigherOrder[K, ALG[_], A, B, Z](
    e1: Either[List[(Either[HigherOrderValue[K, ALG, A], ALG[A]], ExtractionError[K])], A],
    e2: Either[List[(Either[HigherOrderValue[K, ALG, A], ALG[A]], ExtractionError[K])], B])(
    f: (A, B) => Z)
    : Either[List[(Either[HigherOrderValue[K, ALG, A], ALG[A]], ExtractionError[K])], Z] = {
    (e1, e2) match {
      case (Left(err1), Left(err2)) => Left(err1 ::: err2)
      case (Left(err1), _)          => Left(err1)
      case (_, Left(err2))          => Left(err2)
      case (Right(a), Right(b))     => Right(f(a, b))
    }
  }

  // Imported here because there are uses of Scala List's `::` above.
  import shapeless.::

  /**
    * Accumulates error on the left or combines success on the right, just
    * like Applicative.map2 if Either was a Validation.
    */
  def eitherMap2Nullable[K, A, B, Z](
    e1: Either[ExtractionErrors[K], CanBeOmitted[K, A]],
    e2: Either[ExtractionErrors[K], CanBeOmitted[K, B]])(
    f: (A, B) => Z): Either[ExtractionErrors[K], CanBeOmitted[K, Z]] = {
    (e1, e2) match {
      case (Left(err1), Left(err2)) => Left(err1 ::: err2)
      case (Left(err1), _)          => Left(err1)
      case (_, Left(err2))          => Left(err2)
      case (Right(a), Right(b)) =>
        (a, b) match {
          case (Left(nullA), Left(nullB))     => Right(Left(nullA ::: nullB))
          case (Left(nullA), _)               => Right(Left(nullA))
          case (_, Left(nullB))               => Right(Left(nullB))
          case (Right(valueA), Right(valueB)) => Right(Right(f(valueA, valueB)))
        }
    }
  }

  /**
    * Accumulates error on the left or combines success on the right, just
    * like Applicative.map2 if Either was a Validation.
    */
  def eitherOmittedUnaryPrepend[K, A, B <: HList, Z](
    e1: Either[ExtractionErrors[K], CanBeOmitted[K, A]],
    e2: Either[ExtractionErrors[K], B])(implicit uct: UnaryTCConstraint[B, CanBeOmitted[String, *]])
    : Either[ExtractionErrors[K], CanBeOmitted[K, A] :: B] = {
    implicit val uc = uct
    (e1, e2) match {
      case (Left(err1), Left(err2)) => Left(err1 ::: err2)
      case (Left(err1), _)          => Left(err1)
      case (_, Left(err2))          => Left(err2)
      case (Right(a), Right(b))     => Right(a :: b)
    }
  }

}
