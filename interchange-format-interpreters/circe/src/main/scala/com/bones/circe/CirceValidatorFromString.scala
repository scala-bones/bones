package com.bones.circe

import com.bones.data.Error.{ExtractionErrors, ParsingError}
import com.bones.data.values.AnyAlg
import com.bones.interpreter.validator.Validator
import io.circe.Json

object CirceValidatorFromString extends Validator[String, AnyAlg, Json, String] {
  private def fromString(input: String): Either[List[ParsingError[String]], Json] = {
    io.circe.parser
      .parse(input)
      .left
      .map(x => List(ParsingError(x.message)))
  }

  override def validateWithPath(
    in: String,
    path: List[String]
  ): Either[ExtractionErrors[String], Json] = fromString(in)
}
