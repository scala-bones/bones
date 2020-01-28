package com.bones

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError}

/**
  * A collection of helper functions used by Bones.
  */
object Util {

  /** Convert a String to a UUID returning Left[NoneEmptyList[ExtractionError],UUID]
   * if there is a failure in conversion */
  def stringToUuid(uuidString: String, path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    try {
      Right(UUID.fromString(uuidString))
    } catch {
      case e: IllegalArgumentException =>
        Left(NonEmptyList.one(CanNotConvert(path, uuidString, classOf[UUID], Some(e))))
    }

    /**
     * Convert the String to a LocalDate returning Either[NonEmptyList[ExtractionError],LocalDate]
     */
  def stringToLocalDate(input: String,
    dateFormat: DateTimeFormatter,
    path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDate] =
  try {
    Right(LocalDate.parse(input, dateFormat))
  } catch {
    case e: DateTimeParseException =>
      Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDate], Some(e))))
  }

  def stringToLocalTime(input: String,
                        dateFormat: DateTimeFormatter,
                        path: List[String])
  : Either[NonEmptyList[ExtractionError], LocalTime] =
    try {
      Right(LocalTime.parse(input, dateFormat))
    } catch {
      case e: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDate], Some(e))))
    }


    /** Convert the String to a LocalDateTime.
     *  @return Left[NonEmptyList[ExtractionError],String]
     */
    def stringToLocalDateTime(input: String,
                            dateFormat: DateTimeFormatter,
                            path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    try {
      Right(LocalDateTime.parse(input, dateFormat))
    } catch {
      case e: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime], Some(e))))
    }

    /**
     * Convert the String to a BigDecimal returning Either[NonEmptyList[ExtractionError],BigDecimal]
     */
    def stringToBigDecimal(
      input: String,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    try {
      Right(BigDecimal(input))
    } catch {
      case e: NumberFormatException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[BigDecimal], Some(e))))
    }

    /** Convert the String to an Enumeration using [[Enumeration.withName]] returning Left[NonEmptyList[ExtractionError],Object]
     * if there is an parse error.
     */
    def stringToEnumeration[E<:Enumeration,V](str: String,
                             path: List[String],
                             enumeration: E)(implicit manifest: Manifest[V])
    : Either[NonEmptyList[CanNotConvert[String, V]], enumeration.Value] =
    try {
      val clazz = manifest.runtimeClass.asInstanceOf[Class[enumeration.Value]]
      Right(clazz.cast(enumeration.withName(str)))
    } catch {
      case e: NoSuchElementException =>
        Left(NonEmptyList.one(CanNotConvert(path, str, manifest.runtimeClass.asInstanceOf[Class[V]], Some(e))))
    }

    /** Convert the string to an Enum using [[Enumeration.withName]] returning Either[NonEmptyList[ExtractionError],A]
     * if there is an parse error.
     */
    def stringToEnum[A <: Enum[A]:Manifest](
      str: String,
      path: List[String],
      enums: List[A]): Either[NonEmptyList[CanNotConvert[String, A]], A] = {
      val manifestA = manifest[A]
      enums
        .find(_.toString == str)
        .toRight(NonEmptyList.one(CanNotConvert[String,A](path, str, manifestA.runtimeClass.asInstanceOf[Class[A]], None)))
    }

  /** Converts a long to a LocalDate.  Never fails */
  def longToLocalDate: (Long, List[String]) => Either[NonEmptyList[ExtractionError], LocalDate] =
    (l,p) => Right(LocalDate.ofEpochDay(l))

  def longToLocalTime: (Long, List[String]) => Either[NonEmptyList[ExtractionError], LocalTime] =
    (l,p) => Right(LocalTime.ofNanoOfDay(l))


  /**
   * Accumulates error on the left or combines success on the right, just
   * like Applicative.map2 if Either was a Validation.
   */     
  def eitherMap2[A, B, Z](e1: Either[NonEmptyList[ExtractionError], A],
                          e2: Either[NonEmptyList[ExtractionError], B])(
      f: (A, B) => Z): Either[NonEmptyList[ExtractionError], Z] = {
    (e1, e2) match {
      case (Left(err1), Left(err2)) => Left(err1 concatNel err2)
      case (Left(err1), _)          => Left(err1)
      case (_, Left(err2))          => Left(err2)
      case (Right(a), Right(b))     => Right(f(a, b))
    }
  }

}
