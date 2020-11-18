package com.bones

import java.nio.charset.Charset

import com.bones.bson.values.{defaultBsonEncoderInterpreter, defaultBsonValidatorInterpreter}
import com.bones.bson.{bsonResultToBytes, fromByteArray}
import com.bones.circe.{IsoCirceEncoderInterpreter, IsoCirceValidatorInterpreter}
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.http.common._

object Config {

  val charset: Charset = Charset.forName("UTF-8")

  val jsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](kvp: KvpCollection[String, DefaultValues, A]): EncoderFunc[A] =
      IsoCirceEncoderInterpreter(com.bones.circe.values.defaultEncoders)
        .generateEncoder(kvp)
        .andThen(_.noSpaces.getBytes(charset))

    override def generateValidator[A](
      kvp: KvpCollection[String, DefaultValues, A]): ValidatorFunc[A] =
      IsoCirceValidatorInterpreter(com.bones.circe.values.defaultValidators)
        .generateByteArrayValidator(kvp, charset)
  }

  val bsonInterpreter = new Interpreter[String, DefaultValues] {
    override def generateEncoder[A](kvp: KvpCollection[String, DefaultValues, A]): EncoderFunc[A] =
      defaultBsonEncoderInterpreter
        .generateEncoder(kvp)
        .andThen(bsonResultToBytes)

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
    override def generateEncoder[A](kvp: KvpCollection[String, DefaultValues, A]): EncoderFunc[A] =
      com.bones.protobuf.values.defaultEncoder.generateProtobufEncoder(kvp)

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
