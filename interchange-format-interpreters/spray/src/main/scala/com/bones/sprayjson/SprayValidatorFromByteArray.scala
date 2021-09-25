package com.bones.sprayjson

import com.bones.data.Error.{ExtractionErrors, ParsingError}
import com.bones.data.values.AnyAlg
import com.bones.interpreter.validator.Validator
import spray.json.JsValue
import spray.json._

import java.nio.charset.{Charset, StandardCharsets}
import scala.util.{Success, Try}

case class SprayValidatorFromByteArray(charset: Charset = StandardCharsets.UTF_8)
    extends Validator[String, AnyAlg, JsValue, Array[Byte]] {

  private def fromByteArray(
    arr: Array[Byte],
    charSet: Charset
  ): Either[List[ParsingError[String]], JsValue] = {
    val input = new String(arr, charSet)
    Try { input.parseJson }.toEither.left.map(ex => List(ParsingError(ex.getMessage, Some(ex))))
  }

  override def validateWithPath(
    in: Array[Byte],
    path: List[String]
  ): Either[ExtractionErrors[String], JsValue] = fromByteArray(in, charset)

}
