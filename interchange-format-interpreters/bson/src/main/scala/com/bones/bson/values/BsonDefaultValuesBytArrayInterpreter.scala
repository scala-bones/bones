package com.bones.bson.values

import com.bones.bson.{bsonResultToBytes, fromByteArray}
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common.Interpreter
import com.bones.interpreter.encoder.Encoder
import com.bones.interpreter.validator.Validator

import java.nio.charset.{Charset, StandardCharsets}

case class BsonDefaultValuesBytArrayInterpreter(charset: Charset = StandardCharsets.UTF_8)
    extends Interpreter[String, DefaultValues] {
  override def generateEncoder[A](
    kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
    defaultBsonEncoderInterpreter
      .generateEncoder(kvp)
      .map(bsonResultToBytes)

  override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
    : Validator[String, DefaultValues, A, Array[Byte]] = {
    val f = defaultBsonValidatorInterpreter
      .generateValidator(kvp)
    (bytes: Array[Byte], path: List[String]) =>
      {
        fromByteArray(bytes).flatMap(bson => f.validateWithPath(bson, path))
      }
  }
}
