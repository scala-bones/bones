package com.bones.argonaut

import java.nio.charset.Charset

import argonaut.Argonaut._
import argonaut._
import cats.data.NonEmptyList
import cats.implicits._
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredValue, WrongTypeError}
import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data.KvpValue.Path
import com.bones.data.{KeyValueDefinition, _}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.{InterchangeFormatValidator, NoAlgebraValidator}
import com.bones.syntax.NoAlgebra

import scala.util.control.NonFatal

/**
  * Module responsible for converting argonaut JSON input into values with validation checks.
  * See [KvpInterchangeFormatValidatorInterpreter.validatorFromSchema] for the entry point.
  */
trait ArgonautValidatorInterpreter extends KvpInterchangeFormatValidatorInterpreter[Json] {

  override def isEmpty(json: Json): JsonBoolean = json.isNull

  def byteArrayFuncFromSchema[A](schema: BonesSchema[NoAlgebra, A], charset: Charset) =
    byteArrayFuncFromCustomSchema[NoAlgebra, A](schema, NoAlgebraValidator(), charset)

  def byteArrayFuncFromCustomSchema[ALG[_], A](
    schema: BonesSchema[ALG, A],
    customValidator: InterchangeFormatValidator[ALG, Json],
    charset: Charset): Array[Byte] => Either[NonEmptyList[ExtractionError], A] = {
    val fromSchemaFunction = validatorFromCustomSchema(schema, customValidator)
    bytes =>
      {
        try {
          val str = new String(bytes, charset)
          Parse
            .parse(str)
            .left
            .map(str => NonEmptyList.one(ParsingError(str)))
            .flatMap(fromSchemaFunction(_))
        } catch {
          case NonFatal(ex) =>
            Left(NonEmptyList.one(ParsingError(ex.getMessage, Some(ex))))
        }
      }
  }

  def fromByteArray(arr: Array[Byte], charset: Charset): Either[ExtractionError, Json] =
    Parse.parse(new String(arr, charset)).left.map(err => ParsingError(err))

  override def headValue[ALG[_], A](
    in: Json,
    kv: KeyValueDefinition[ALG, A],
    headInterpreter: (Option[Json], List[String]) => Either[NonEmptyList[ExtractionError], A],
    path: List[String]): Either[NonEmptyList[ExtractionError], A] = {

    in.obj
      .toRight[NonEmptyList[ExtractionError]](
        NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass, None)))
      .flatMap(
        _.toList
          .find(f => f._1 === kv.key)
          .toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(RequiredValue(path, kv.op))))
      .flatMap(j => headInterpreter.apply(Some(j._2), path))
  }

  override def extractString[ALG[_], A](op: CoproductDataDefinition[ALG, A], clazz: Class[_])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.string.toRight(NonEmptyList.one(WrongTypeError(path, clazz, in.getClass, None)))

  override def extractShort[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: Path): Either[NonEmptyList[ExtractionError], Short] =
    in.number
      .flatMap(n => n.toShort)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Short], in.getClass, None)))

  override def extractInt[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.number
      .flatMap(n => n.toInt)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass, None)))

  override def extractLong[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], Long] =
    in.number
      .flatMap(_.toLong)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass, None)))

  override def extractBool[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], JsonBoolean] =
    in.bool.toRight(NonEmptyList.one(WrongTypeError(path, classOf[Boolean], in.getClass, None)))

  override def extractArray[ALG[_], A](op: ListData[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.array.toRight(NonEmptyList.one(WrongTypeError(path, classOf[Array[_]], in.getClass, None)))

  override def extractFloat[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.number
      .flatMap(n => n.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Byte], in.getClass, None)))

  override def extractDouble[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.number
      .flatMap(n => n.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Byte], in.getClass, None)))

  override def extractBigDecimal[ALG[_], A](op: CoproductDataDefinition[ALG, A])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.number
      .map(_.toBigDecimal)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[BigDecimal], in.getClass, None)))

  override def invalidValue[T](
    in: Json,
    expected: Class[T],
    path: List[String]): Left[NonEmptyList[ExtractionError], Nothing] = {
    val invalid = in.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid, None)))
  }

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.obj
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.string
    } yield value

}
