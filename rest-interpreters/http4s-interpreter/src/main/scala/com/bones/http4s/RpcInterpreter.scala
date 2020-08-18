package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import com.bones.circe.IsoCirceEncoderAndValidatorInterpreter
import com.bones.data.PrimitiveWrapperValue
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.interpreter.{InterchangeFormatEncoderValue, InterchangeFormatValidatorValue}
import com.bones.protobuf._
import io.circe.Json
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import reactivemongo.bson.BSONValue

class RpcInterpreter[ALG[_], ID: Manifest](
  path: String,
  jsonValidator: InterchangeFormatValidatorValue[ALG, Json],
  jsonEncoder: InterchangeFormatEncoderValue[ALG, Json],
  bsonValidator: InterchangeFormatValidatorValue[ALG, BSONValue],
  bsonEncoder: InterchangeFormatEncoderValue[ALG, BSONValue],
  protobufValidator: ProtobufValidatorValue[ALG],
  protobufEncoder: ProtobufEncoderValue[ALG],
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
    inputSchema: PrimitiveWrapperValue[ALG, A],
    errorSchema: PrimitiveWrapperValue[ALG, E],
    outputSchema: PrimitiveWrapperValue[ALG, B]
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
