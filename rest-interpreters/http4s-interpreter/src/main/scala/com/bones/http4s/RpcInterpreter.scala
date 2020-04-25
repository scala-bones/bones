package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import com.bones.circe.IsoCirceEncoderAndValidatorInterpreter
import com.bones.data.BonesSchema
import com.bones.http4s.ClassicCrudInterpreter.{CustomInterpreter, ProtobufEncoderInterpreter}
import com.bones.protobuf.{ProtobufSequentialEncoderInterpreter, ProtobufSequentialValidatorInterpreter, ProtobufUtcSequentialEncoderAndValidator}
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import reactivemongo.bson.BSONValue

class RpcInterpreter[ALG[_], E, F[_], ID: Manifest](
  path: String,
  charset: java.nio.charset.Charset = StandardCharsets.UTF_8,
  customJsonInterpreter: CustomInterpreter[ALG, Json],
  customBsonInterpreter: CustomInterpreter[ALG, BSONValue],
  customProtobufInterpreter: ProtobufEncoderInterpreter[ALG],
//  customSwaggerInterpreter: CustomSwaggerInterpreter[ALG],
  pathStringToId: String => Either[E, ID]
) {

  val encodeToCirceInterpreter = IsoCirceEncoderAndValidatorInterpreter
  val validatedFromCirceInterpreter = IsoCirceEncoderAndValidatorInterpreter
  val protobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
    ProtobufUtcSequentialEncoderAndValidator
  val protobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
    ProtobufUtcSequentialEncoderAndValidator


  def create[A,B](
    createF: A => F[Either[E, B]],
    inputSchema: BonesSchema[ALG,A],
    errorSchema: BonesSchema[ALG,E],
    outputSchema: BonesSchema[ALG,B]
  )(implicit F: Sync[F], H: Http4sDsl[F]): List[HttpRoutes[F]] =
    BaseCrudInterpreter.httpPostRoutes(
      path,
      pathStringToId,
      createF,
      inputSchema,
      errorSchema,
      outputSchema,
      validatedFromCirceInterpreter,
      encodeToCirceInterpreter,
      protobufSequentialInputInterpreter,
      protobufSequentialOutputInterpreter,
      customJsonInterpreter,
      customBsonInterpreter,
      customProtobufInterpreter,
      charset
    )

}
