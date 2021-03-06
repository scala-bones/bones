package com.bones.circe.values

import com.bones.circe.{CirceValidatorFromByteArray, IsoCirceEncoderInterpreter}
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common.Interpreter
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator

import java.nio.charset.StandardCharsets

object Interpreter {

  object CirceDefaultValuesByteArrayInterpreter extends Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      IsoCirceEncoderInterpreter(com.bones.circe.values.defaultEncoders)
        .generateEncoder(kvp)
        .map(_.noSpaces.getBytes(StandardCharsets.UTF_8))

    override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
      : Validator[String, DefaultValues, A, Array[Byte]] = {
      CirceValidatorFromByteArray()
        .andThen(isoCirceValidatorInterpreter.generateValidator(kvp))
    }
  }
}
