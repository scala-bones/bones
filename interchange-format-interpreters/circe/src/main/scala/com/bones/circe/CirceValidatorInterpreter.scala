package com.bones.circe

import java.nio.charset.Charset

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, ParsingError, WrongTypeError}
import com.bones.data.{KeyDefinition, _}
import com.bones.interpreter.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  KvpInterchangeFormatValidatorInterpreter
}
import io.circe.Json

/**
  * Module responsible for converting circe JSON input into values with validation checks.
  * See [KvpInterchangeFormatValidatorInterpreter.validatorFromSchema] for the entry point.
  */
trait CirceValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatValidatorInterpreter[ALG, Json] {

  val coproductTypeKey: String
  val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, Json]
  val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[Json]

  override def isEmpty(json: Json): Boolean = json.isNull
  override def invalidValue[T](
    json: Json,
    typeName: String,
    path: List[String]): Left[NonEmptyList[WrongTypeError[String, T]], Nothing] = {
    val invalid = json.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, typeName, invalid.getSimpleName, None)))
  }

  override def headValue[A](
    in: Json,
    kv: KeyDefinition[String, ALG, A],
    headInterpreter: (
      Option[Json],
      List[String]) => Either[NonEmptyList[ExtractionError[String]], A],
    path: List[String]): Either[NonEmptyList[ExtractionError[String]], A] =
    in.asObject match {
      case Some(jsonObj) =>
        val fields = jsonObj.toList
        headInterpreter(fields.find(_._1 == kv.key).map(_._2), path)
      case None =>
        Left(NonEmptyList.one(WrongTypeError(path, kv.typeName, in.getClass.getSimpleName, None)))
    }

  def generateByteArrayValidator[A](
    schema: KvpCollection[String, ALG, A],
    charset: Charset
  ): Array[Byte] => Either[NonEmptyList[ExtractionError[String]], A] = {
    val f = fromKvpCollection(schema)
    bytes =>
      fromByteArray(bytes, charset).flatMap(f(_, List.empty))
  }

  private def fromByteArray(
    arr: Array[Byte],
    charSet: Charset): Either[NonEmptyList[ParsingError[String]], Json] = {
    val input = new String(arr, charSet)
    io.circe.parser
      .parse(input)
      .left
      .map(x => NonEmptyList.one(ParsingError(x.message)))
  }

}
