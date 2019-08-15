package com.bones

import java.time.{LocalDate, LocalDateTime}
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError}

object Util {

  /** Convert a String to a UUID returning Left[NoneEmptyList[ExtractionError]]
   * if there is a failure in conversion */
  def stringToUuid(
      uuidString: String,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    try {
      Right(UUID.fromString(uuidString))
    } catch {
      case _: IllegalArgumentException =>
        Left(NonEmptyList.one(CanNotConvert(path, uuidString, classOf[UUID])))
    }

    /** Convert the String to a LocalDate returning Left[NonEmptyList[ExtractionError]] 
     * if there is an parse error.
     */
  def stringToLocalDate(input: String,
                        dateFormat: DateTimeFormatter,
                        path: List[String])
  : Either[NonEmptyList[ExtractionError], LocalDate] =
    try {
      Right(LocalDate.parse(input, dateFormat))
    } catch {
      case _: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDate])))
    }

    /** Convert the String to a LocalDateTime returning Left[NonEmptyList[ExtractionError]] 
     * if there is an parse error.
     */
    def stringToLocalDateTime(input: String,
                            dateFormat: DateTimeFormatter,
                            path: List[String])
    : Either[NonEmptyList[ExtractionError], LocalDateTime] =
    try {
      Right(LocalDateTime.parse(input, dateFormat))
    } catch {
      case _: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime])))
    }

    /** Convert the String to a BigDecimal returning Left[NonEmptyList[ExtractionError]] 
     * if there is an parse error.
     */
    def stringToBigDecimal(
      input: String,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    try {
      Right(BigDecimal(input))
    } catch {
      case _: NumberFormatException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[BigDecimal])))
    }

    /** Convert the String to an Enumeration using [[Enumeration.withName]] returning Left[NonEmptyList[ExtractionError]] 
     * if there is an parse error.
     */
    def stringToEnumeration[A](str: String,
                             path: List[String],
                             enumeration: Enumeration,
                             manifest: Manifest[A])
    : Either[NonEmptyList[CanNotConvert[String, Object]], A] =
    try {
      val clazz = manifest.runtimeClass.asInstanceOf[Class[A]]
      Right(clazz.cast(enumeration.withName(str)))
    } catch {
      case _: NoSuchElementException =>
        Left(NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))
    }

    /** Convert the string to an Enum using [[Enumeration.withName]] returning Left[NonEmptyList[ExtractionError]] 
     * if there is an parse error.
     */
    def stringToEnum[A <: Enum[A]](
      str: String,
      path: List[String],
      enums: List[A]): Either[NonEmptyList[CanNotConvert[String, Object]], A] =
    enums
      .find(_.toString == str)
      .toRight(NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))

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
