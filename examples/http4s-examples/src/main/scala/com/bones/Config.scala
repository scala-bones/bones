package com.bones

import java.nio.charset.Charset

import com.bones.bson.values.{defaultBsonEncoderInterpreter, defaultBsonValidatorInterpreter}
import com.bones.bson.{bsonResultToBytes, fromByteArray}
import com.bones.circe.values.isoCirceValidatorInterpreter
import com.bones.circe.{
  CirceFromByteArray,
  IsoCirceEncoderInterpreter,
  IsoCirceValidatorInterpreter
}
import com.bones.data.Error.ExtractionErrors
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common._
import com.bones.interpreter.{Encoder, Validator}

object Config {

  val charset: Charset = Charset.forName("UTF-8")

  val jsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      IsoCirceEncoderInterpreter(com.bones.circe.values.defaultEncoders)
        .generateEncoder(kvp)
        .map(_.noSpaces.getBytes(charset))

    override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
      : Validator[String, DefaultValues, A, Array[Byte]] = {
      CirceFromByteArray(charset).flatMap(
        isoCirceValidatorInterpreter.generateValidator(kvp)
      )
    }
  }

  val bsonInterpreter = new Interpreter[String, DefaultValues] {
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

  val protobufInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      com.bones.protobuf.values.defaultEncoder.generateProtobufEncoder(kvp).apply(_)

    override def generateValidator[A](kvp: KvpCollection[String, DefaultValues, A])
      : Validator[String, DefaultValues, A, Array[Byte]] =
      new Validator[String, DefaultValues, A, Array[Byte]] {
        override def validateWithPath(
          in: Array[Byte],
          path: List[String]): Either[ExtractionErrors[String], A] =
          com.bones.protobuf.values.defaultUtcValidator.fromCustomBytes(kvp).apply(in)
      }

  }

  val defaultContentType = Content("application/json", jsonInterpreter)
  val supportedContentTypes = Set(
    Content("application/ubjson", bsonInterpreter),
    Content("application/protobuf", protobufInterpreter)
  )

  val interpreterConfig = ContentInterpreters(defaultContentType, supportedContentTypes)

}
