package com.bones.skeleton.tapir.circe

import java.nio.charset.{Charset, StandardCharsets}

import com.bones.circe.CirceValidatorFromString
import com.bones.circe.values.{isoCirceEncoderInterpreter, isoCirceValidatorInterpreter}
import com.bones.data.Error.{ExtractionErrors, WrongTypeError}
import com.bones.data.KvpCollection
import com.bones.data.values.DefaultValues
import com.bones.interpreter.validator.{ListValidator, Validator}
import com.bones.interpreter.encoder.{Encoder, ListEncoder}
import com.bones.tapir.BonesToTapirCodec
import com.bones.tapir.values.defaultTransformation
import io.circe.Json
import io.circe.Json.JArray
import sttp.tapir.{Codec, CodecFormat, EndpointIO, RawBodyType, Schema}

object DefaultAlgebras {

  val charset: Charset = StandardCharsets.UTF_8

  def jsonBody[T](
    kvpCollection: KvpCollection[String, DefaultValues, T]): EndpointIO.Body[String, T] = {
    val (_, codec) = createTapirSchemaAndCodec(kvpCollection)
    EndpointIO.Body(RawBodyType.StringBody(charset), codec, EndpointIO.Info.empty)
  }

  def jsonBodyList[T](
    kvpCollection: KvpCollection[String, DefaultValues, T]): EndpointIO.Body[String, List[T]] = {
    val (_, codec) = createTapirSchemaAndCodec(kvpCollection)
//    EndpointIO.Body(RawBodyType.StringBody(charset), codec, EndpointIO.Info.empty)
    ???
  }

  /**
    * Create Tapir bindings from
    * a Bones KvpCollection
    * @param kvpCollection The schema representing the value
    * @tparam A The "high level value" as per Tapir's Codec.
    * @return A schema representing the data and a coded for the encoding, decoding.
    */
  def createTapirSchemaAndCodec[A](kvpCollection: KvpCollection[String, DefaultValues, A])
    : (Schema[A], Codec[String, A, CodecFormat.Json]) = {
    val schema = defaultTransformation.kvpToSchema(kvpCollection)
    val encoder = createEncoder(kvpCollection)
    val validator = createValidator(kvpCollection)
    (schema, BonesToTapirCodec.encoderValidatorToTapirCodec(schema, encoder, validator))
  }

  def createTapirSchemaAndCodecList[A](kvpCollection: KvpCollection[String, DefaultValues, A])
    : (Schema[A], Codec[String, List[A], CodecFormat.Json]) = {
//    val schema = defaultTransformation.kvpToSchema(kvpCollection)
//    val encoder = createListEncoder(kvpCollection)
//    val validator = createListValidator(kvpCollection)
//    (schema, BonesToTapirCodec.encoderValidatorToTapirCodec(schema, encoder, validator))
    ???
  }

  val toList: Json => Either[ExtractionErrors[String], List[Json]] = ???
//  {
//    case JArray(value) => Right(value.toList)
//    case x             => Left(List(WrongTypeError(List.empty, "Array", x.toString, None)))
//  }

  def createListValidator[A](kvpCollection: KvpCollection[String, DefaultValues, A])
    : Validator[String, DefaultValues, List[A], String] = {
    CirceValidatorFromString
      .andThen(ListValidator(isoCirceValidatorInterpreter.generateValidator(kvpCollection), toList))
  }

  def createValidator[A](kvpCollection: KvpCollection[String, DefaultValues, A])
    : Validator[String, DefaultValues, A, String] = {
    CirceValidatorFromString.andThen(
      isoCirceValidatorInterpreter.generateValidator(kvpCollection)
    )
  }

  val fromList: List[Json] => Json = list => ??? //JArray(list.toVector)

  def createListEncoder[A](kvpCollection: KvpCollection[String, DefaultValues, A])
    : Encoder[DefaultValues, List[A], String] = {
    ListEncoder(
      isoCirceEncoderInterpreter
        .generateEncoder(kvpCollection),
      fromList)
      .map(_.noSpaces)
  }

  def createEncoder[A](
    kvpCollection: KvpCollection[String, DefaultValues, A]): Encoder[DefaultValues, A, String] = {
    isoCirceEncoderInterpreter
      .generateEncoder(kvpCollection)
      .map(_.noSpaces)
  }

}
