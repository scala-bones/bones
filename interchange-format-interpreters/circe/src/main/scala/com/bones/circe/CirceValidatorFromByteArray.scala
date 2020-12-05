package com.bones.circe

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.data.Error.{ExtractionErrors, ParsingError}
import com.bones.data.values.AnyAlg
import com.bones.interpreter.validator.Validator
import io.circe.Json

case class CirceValidatorFromByteArray(charset: Charset = StandardCharsets.UTF_8)
    extends Validator[String, AnyAlg, Json, Array[Byte]] {

  private def fromByteArray(
    arr: Array[Byte],
    charSet: Charset): Either[List[ParsingError[String]], Json] = {
    val input = new String(arr, charSet)
    io.circe.parser
      .parse(input)
      .left
      .map(x => List(ParsingError(x.message)))
  }

  override def validateWithPath(
    in: Array[Byte],
    path: List[String]): Either[ExtractionErrors[String], Json] = fromByteArray(in, charset)

}
