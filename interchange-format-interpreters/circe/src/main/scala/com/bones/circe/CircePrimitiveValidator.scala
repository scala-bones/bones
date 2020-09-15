package com.bones.circe

import cats.data.NonEmptyList
import com.bones.Path
import com.bones.data.Error.{ExtractionError, RequiredValue, WrongTypeError}
import com.bones.data.ListData
import com.bones.interpreter.InterchangeFormatPrimitiveValidator
import io.circe.Json

object CircePrimitiveValidator extends InterchangeFormatPrimitiveValidator[Json] {

  override def extractString[ALG2[_], A](op: ALG2[A], typeName: String)(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.asString.toRight(determineError(in, op, typeName, path))

  override def extractInt[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.asNumber
      .flatMap(_.toInt)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Int", in.getClass.getSimpleName, None)))

  override def extractFloat[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.asNumber
      .map(_.toDouble.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Float", in.getClass.getSimpleName, None)))

  override def extractDouble[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.asNumber
      .map(_.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Double", in.getClass.getSimpleName, None)))

  override def extractLong[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber
      .flatMap(_.toLong)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Long", in.getClass.getSimpleName, None)))

  override def extractShort[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Short] =
    in.asNumber
      .flatMap(_.toShort)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Short", in.getClass.getSimpleName, None)))

  override def extractBool[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in.asBoolean.toRight(determineError(in, op, "Boolean", path))

  override def extractArray[ALG2[_], A](
    op: ListData[ALG2, A])(in: Json, path: Path): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(determineError(in, Left(op), op.typeNameOfT, path))

  override def extractBigDecimal[ALG2[_], A](
    op: ALG2[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(determineError(in, op, "BigDecimal", path))

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.asObject
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.asString
    } yield value

  protected def determineError[ALG2[_], A](
    in: Json,
    op: ALG2[A],
    typeName: String,
    path: List[String]): NonEmptyList[ExtractionError] = {
    val error =
      if (in.isNull) RequiredValue(path, typeName)
      else WrongTypeError(path, typeName, in.getClass.getSimpleName, None)
    NonEmptyList.one(error)
  }

}
