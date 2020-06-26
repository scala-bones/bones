package com.bones.circe

import java.nio.charset.Charset

import cats.data.NonEmptyList
import com.bones.Path
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredValue, WrongTypeError}
import com.bones.data.{KeyValueDefinition, _}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import io.circe.Json

/**
  * Module responsible for converting circe JSON input into values with validation checks.
  * See [KvpInterchangeFormatValidatorInterpreter.validatorFromSchema] for the entry point.
  */
trait CirceValidatorInterpreter extends KvpInterchangeFormatValidatorInterpreter[Json] {

  override def isEmpty(json: Json): Boolean = json.isNull

  def byteArrayFuncFromSchema[ALG[_], A](
    schema: BonesSchema[ALG, A],
    charset: Charset,
    validatorInterpreter: InterchangeFormatValidator[ALG, Json]
  ): Array[Byte] => Either[NonEmptyList[ExtractionError], A] = {
    val f = validatorFromCustomSchema(schema, validatorInterpreter)
    bytes =>
      fromByteArray(bytes, charset).flatMap(f(_))
  }

  def fromByteArray(
    arr: Array[Byte],
    charSet: Charset): Either[NonEmptyList[ParsingError], Json] = {
    val input = new String(arr, charSet)
    io.circe.parser
      .parse(input)
      .left
      .map(x => NonEmptyList.one(ParsingError(x.message)))
  }

  override def invalidValue[T](
    json: Json,
    expected: Class[T],
    path: List[String]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = json.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid, None)))
  }

  protected def determineError[ALG[_], A](
    in: Json,
    op: ALG[A],
    expectedType: Class[_],
    path: List[String]): NonEmptyList[ExtractionError] = {
    val error =
      if (in.isNull) RequiredValue(path, Right(op))
      else WrongTypeError(path, expectedType, in.getClass, None)
    NonEmptyList.one(error)
  }

  override def headValue[ALG[_], A](
    in: Json,
    kv: KeyValueDefinition[ALG, A],
    headInterpreter: (Option[Json], List[String]) => Either[NonEmptyList[ExtractionError], A],
    path: List[String]): Either[NonEmptyList[ExtractionError], A] =
    in.asObject match {
      case Some(jsonObj) =>
        val fields = jsonObj.toList
        headInterpreter(fields.find(_._1 == kv.key).map(_._2), path)
      case None =>
        Left(NonEmptyList.one(WrongTypeError(path, classOf[Object], in.getClass, None)))
    }

  override def extractString[ALG[_], A](op: ALG[A], clazz: Class[_])(
    in: Json,
    path: List[String]): Either[NonEmptyList[ExtractionError], String] =
    in.asString.toRight(determineError(in, op, clazz, path))

  override def extractInt[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Int] =
    in.asNumber
      .flatMap(_.toInt)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Int], in.getClass, None)))

  override def extractFloat[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Float] =
    in.asNumber
      .map(_.toDouble.toFloat)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Float], in.getClass, None)))

  override def extractDouble[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Double] =
    in.asNumber
      .map(_.toDouble)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Double], in.getClass, None)))

  override def extractLong[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber
      .flatMap(_.toLong)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Long], in.getClass, None)))

  override def extractShort[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Short] =
    in.asNumber
      .flatMap(_.toShort)
      .toRight(NonEmptyList.one(WrongTypeError(path, classOf[Short], in.getClass, None)))

  override def extractBool[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in.asBoolean.toRight(determineError(in, op, classOf[Boolean], path))

  override def extractArray[ALG[_], A](
    op: ListData[ALG, A])(in: Json, path: Path): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(determineError(in, Left(op), classOf[List[A]], path))

  override def extractBigDecimal[ALG[_], A](
    op: ALG[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(determineError(in, op, classOf[BigDecimal], path))

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.asObject
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.asString
    } yield value

}
