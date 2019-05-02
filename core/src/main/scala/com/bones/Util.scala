package com.bones

import java.time.ZonedDateTime
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.{Date, UUID}

import cats.data.NonEmptyList
import com.bones.data.Error.{CanNotConvert, ExtractionError}

object Util {

  def stringToUuid(
      uuidString: String,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    try {
      Right(UUID.fromString(uuidString))
    } catch {
      case _: IllegalArgumentException =>
        Left(NonEmptyList.one(CanNotConvert(path, uuidString, classOf[UUID])))
    }

  def stringToZonedDateTime(input: String,
                            dateFormat: DateTimeFormatter,
                            path: List[String])
    : Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    try {
      Right(ZonedDateTime.parse(input, dateFormat))
    } catch {
      case _: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[Date])))
    }

  def stringToBigDecimal(
      input: String,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    try {
      Right(BigDecimal(input))
    } catch {
      case _: NumberFormatException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[BigDecimal])))
    }

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

  def stringToEnum[A <: Enum[A]](
      str: String,
      path: List[String],
      enums: List[A]): Either[NonEmptyList[CanNotConvert[String, Object]], A] =
    enums
      .find(_.toString == str)
      .toRight(NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))

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
