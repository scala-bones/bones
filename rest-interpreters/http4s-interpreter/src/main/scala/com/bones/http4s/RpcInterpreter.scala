package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import com.bones.circe.IsoCirceEncoderAndValidatorInterpreter
import com.bones.data.KvpCollection
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.protobuf.{
  ProtobufSequentialEncoderInterpreter,
  ProtobufSequentialValidatorInterpreter,
  ProtobufUtcSequentialEncoderAndValidator,
  ProtobufValueEncoder,
  ProtobufValueValidator
}
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import reactivemongo.bson.BSONValue

class RpcInterpreter[ALG[_], ID: Manifest](
  path: String,
  jsonValidator: InterchangeFormatValidator[ALG, Json],
  jsonEncoder: InterchangeFormatEncoder[ALG, Json],
  bsonValidator: InterchangeFormatValidator[ALG, BSONValue],
  bsonEncoder: InterchangeFormatEncoder[ALG, BSONValue],
  protobufValidator: ProtobufValueValidator[ALG],
  protobufEncoder: ProtobufValueEncoder[ALG],
  //  customSwaggerInterpreter: CustomSwaggerInterpreter[ALG],
  pathStringToId: String => Either[StringToIdError, ID],
  charset: java.nio.charset.Charset = StandardCharsets.UTF_8
) {

  private val encodeToCirceInterpreter = IsoCirceEncoderAndValidatorInterpreter
  private val validatedFromCirceInterpreter = IsoCirceEncoderAndValidatorInterpreter
  private val protobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
    ProtobufUtcSequentialEncoderAndValidator
  private val protobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
    ProtobufUtcSequentialEncoderAndValidator

  def create[F[_], A, E, B](
    createF: A => F[Either[E, B]],
    inputSchema: KvpCollection[ALG, A],
    errorSchema: KvpCollection[ALG, E],
    outputSchema: KvpCollection[ALG, B]
  )(implicit F: Sync[F], H: Http4sDsl[F]): List[HttpRoutes[F]] =
    BaseCrudInterpreter.httpPostRoutes(
      path,
      createF,
      inputSchema,
      errorSchema,
      outputSchema,
      validatedFromCirceInterpreter,
      encodeToCirceInterpreter,
      protobufSequentialInputInterpreter,
      protobufSequentialOutputInterpreter,
      jsonValidator,
      jsonEncoder,
      bsonValidator,
      bsonEncoder,
      protobufValidator,
      protobufEncoder,
      charset
    )

}
