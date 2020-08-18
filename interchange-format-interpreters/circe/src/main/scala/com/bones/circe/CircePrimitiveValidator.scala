package com.bones.circe

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, RequiredValue, WrongTypeError}
import com.bones.data.ListData
import com.bones.interpreter.InterchangeFormatPrimitiveValidator
import io.circe.Json
import com.bones.Path

object CircePrimitiveValidator extends InterchangeFormatPrimitiveValidator[Json] {

  override def extractString[ALG2[_], A](op: ALG2[A], clazz: Class[_])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.asString.toRight(determineError(in, op, clazz, path))

  override def extractInt[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.asNumber
      .flatMap(_.toInt)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass, None)))

  override def extractFloat[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.asNumber
      .map(_.toDouble.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Float], in.getClass, None)))

  override def extractDouble[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.asNumber
      .map(_.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Double], in.getClass, None)))

  override def extractLong[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber
      .flatMap(_.toLong)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass, None)))

  override def extractShort[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Short] =
    in.asNumber
      .flatMap(_.toShort)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Short], in.getClass, None)))

  override def extractBool[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in.asBoolean.toRight(determineError(in, op, classOf[Boolean], path))

  override def extractArray[ALG2[_], A](
    op: ListData[ALG2, A])(in: Json, path: Path): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(determineError(in, Left(op), classOf[List[A]], path))

  override def extractBigDecimal[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(determineError(in, op, classOf[BigDecimal], path))

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.asObject
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.asString
    } yield value

  protected def determineError[ALG2[_], A](
    in: Json,
    op: ALG2[A],
    expectedType: Class[_],
    path: List[String]): NonEmptyList[ExtractionError] = {
    val error =
      if (in.isNull) RequiredValue.fromDef(path, Right(op))
      else WrongTypeError(path, expectedType, in.getClass, None)
    NonEmptyList.one(error)
  }
}
