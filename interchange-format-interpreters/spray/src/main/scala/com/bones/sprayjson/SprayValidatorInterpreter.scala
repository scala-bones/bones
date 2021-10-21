package com.bones.sprayjson

import java.nio.charset.Charset

import com.bones.data.Error.{ExtractionError, ParsingError, WrongTypeError}
import com.bones.data.{KeyDefinition, _}
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  KvpInterchangeFormatValidatorInterpreter,
  OptionalInputValidator
}
import spray.json._

import scala.util.Try

/** Module responsible for converting circe JsValue input into values with validation checks. See
  * [KvpInterchangeFormatValidatorInterpreter.validatorFromSchema] for the entry point.
  */
trait SprayValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatValidatorInterpreter[ALG, JsValue] {

  val coproductTypeKey: String
  val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, JsValue]
  val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[JsValue]

  override def isEmpty(value: JsValue): Boolean = value match {
    case JsNull => true
    case _      => false
  }
  override def invalidValue[T](
    value: JsValue,
    typeName: String,
    path: List[String]
  ): Left[List[WrongTypeError[String, T]], Nothing] = {
    val invalid = value match {
      case JsNull       => classOf[Nothing]
      case JsArray(_)   => classOf[Array[_]]
      case JsBoolean(_) => classOf[Boolean]
      case JsNumber(_)  => classOf[Number]
      case JsObject(_)  => classOf[Object]
      case JsString(_)  => classOf[String]
    }

    Left(List(WrongTypeError(path, typeName, invalid.getSimpleName, None)))
  }

  override def headValue[A](
    in: JsValue,
    kv: KeyDefinition[String, ALG, A],
    headInterpreter: OptionalInputValidator[String, ALG, A, JsValue],
    path: List[String]
  ): Either[List[ExtractionError[String]], A] =
    Try(in.asJsObject).toOption match {
      case Some(value) =>
        headInterpreter.validateWithPath(value.fields.find(_._1 == kv.key).map(_._2), path)
      case None =>
        Left(List(WrongTypeError(path, kv.typeName, in.getClass.getSimpleName, None)))
    }

  def generateByteArrayValidator[A](
    schema: KvpCollection[String, ALG, A],
    charset: Charset
  ): Array[Byte] => Either[List[ExtractionError[String]], A] = {
    val encoder = fromKvpCollection(schema)
    bytes => fromByteArray(bytes, charset).flatMap(encoder.validateWithPath(_, List.empty))
  }

  private def fromByteArray(
    arr: Array[Byte],
    charSet: Charset
  ): Either[List[ParsingError[String]], JsValue] = {
    val input = new String(arr, charSet)
    Try(input.parseJson).toEither.left.map(x => List(ParsingError(x.getMessage)))
  }

}
