package com.bones.argonaut

import argonaut.Argonaut.JsonBoolean
import argonaut.Json
import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionErrors, WrongTypeError}
import com.bones.data.ListData
import com.bones.interpreter.InterchangeFormatPrimitiveValidator

object ArgonautPrimitiveValidator extends InterchangeFormatPrimitiveValidator[Json] {
  override def extractString[ALG[_], A](
    dataDefinition: ALG[A],
    typeName: String
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], String] =
    in.string.toRight(
      NonEmptyList.one(WrongTypeError(path, typeName, in.getClass.getSimpleName, None)))

  override def extractShort[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], Short] =
    in.number
      .flatMap(n => n.toShort)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Short", in.getClass.getSimpleName, None)))

  override def extractInt[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], Int] =
    in.number
      .flatMap(n => n.toInt)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Int", in.getClass.getSimpleName, None)))

  override def extractLong[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], Long] =
    in.number
      .flatMap(_.toLong)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Long", in.getClass.getSimpleName, None)))

  override def extractBool[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], JsonBoolean] =
    in.bool.toRight(
      NonEmptyList.one(WrongTypeError(path, "Boolean", in.getClass.getSimpleName, None)))

  override def extractArray[ALG[_], A](
    op: ListData[String, ALG, A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], Seq[Json]] =
    in.array.toRight(
      NonEmptyList.one(WrongTypeError(path, op.typeName, in.getClass.getSimpleName, None)))

  override def extractFloat[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], Float] =
    in.number
      .flatMap(n => n.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Float", in.getClass.getSimpleName, None)))

  override def extractDouble[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], Double] =
    in.number
      .flatMap(n => n.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, "Double", in.getClass.getSimpleName, None)))

  override def extractBigDecimal[ALG[_], A](
    dataDefinition: ALG[A]
  )(in: Json, path: List[String]): Either[ExtractionErrors[String], BigDecimal] =
    in.number
      .map(_.toBigDecimal)
      .toRight(
        NonEmptyList.one(WrongTypeError(path, "BigDecimal", in.getClass.getSimpleName, None)))
  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.obj
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.string
    } yield value

}
