package com.bones

import java.nio.charset.Charset

import com.bones.bson.values.{defaultBsonEncoderInterpreter, defaultBsonValidatorInterpreter}
import com.bones.bson.{bsonResultToBytes, fromByteArray}
import com.bones.circe.{IsoCirceEncoderInterpreter, IsoCirceValidatorInterpreter}
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common._
import com.bones.interpreter.Encoder

object Config {

  val charset: Charset = Charset.forName("UTF-8")

  val jsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      IsoCirceEncoderInterpreter(com.bones.circe.values.defaultEncoders)
        .generateEncoder(kvp)
        .map(_.noSpaces.getBytes(charset))

    override def generateValidator[A](
      kvp: KvpCollection[String, DefaultValues, A]): ValidatorFunc[A] =
      IsoCirceValidatorInterpreter(com.bones.circe.values.defaultValidators)
        .generateByteArrayValidator(kvp, charset)
  }

  val bsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      defaultBsonEncoderInterpreter
        .generateEncoder(kvp)
        .map(bsonResultToBytes)

    override def generateValidator[A](
      kvp: KvpCollection[String, DefaultValues, A]): ValidatorFunc[A] = {
      val f = defaultBsonValidatorInterpreter
        .generateValidator(kvp)
      (bytes: Array[Byte]) =>
        {
          fromByteArray(bytes).flatMap(f)
        }
    }
  }

  val protobufInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](
      kvp: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, Array[Byte]] =
      com.bones.protobuf.values.defaultEncoder.generateProtobufEncoder(kvp).apply(_)

    override def generateValidator[A](
      kvp: KvpCollection[String, DefaultValues, A]): ValidatorFunc[A] =
      com.bones.protobuf.values.defaultUtcValidator.fromCustomBytes(kvp)
  }

  val defaultContentType = Content("application/json", jsonInterpreter)
  val supportedContentTypes = Set(
    Content("application/ubjson", bsonInterpreter),
    Content("application/protobuf", protobufInterpreter)
  )

  val interpreterConfig = ContentInterpreters(defaultContentType, supportedContentTypes)

}
