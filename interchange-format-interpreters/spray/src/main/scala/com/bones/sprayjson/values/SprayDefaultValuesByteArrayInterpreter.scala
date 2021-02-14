package com.bones.sprayjson.values

import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common.Interpreter
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator
import com.bones.sprayjson.SprayValidatorFromByteArray

import java.nio.charset.{Charset, StandardCharsets}

case class SprayDefaultValuesByteArrayInterpreter(charset: Charset = StandardCharsets.UTF_8)
    extends Interpreter[String, DefaultValues] {
  override def generateEncoder[A](
    kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
    com.bones.sprayjson.values.isoSprayEncoderInterpreter
      .generateEncoder(kvp)
      .map(_.compactPrint.getBytes(charset))

  override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
    : Validator[String, DefaultValues, A, Array[Byte]] = {
    val validator = com.bones.sprayjson.values.isoSprayValidatorInterpreter
      .generateValidator(kvp)
    SprayValidatorFromByteArray(charset)
      .andThen(validator)
  }
}
