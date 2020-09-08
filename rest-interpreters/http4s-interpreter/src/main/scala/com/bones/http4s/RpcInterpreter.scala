package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import com.bones.bson.{BsonEncoderInterpreter, BsonValidatorInterpreter}
import com.bones.circe.{CirceEncoderInterpreter, CirceValidatorInterpreter}
import com.bones.data.KvpCollection
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.protobuf._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class RpcInterpreter[ALG[_], ID: Manifest](
  path: String,
  jsonValidator: CirceValidatorInterpreter[ALG],
  jsonEncoder: CirceEncoderInterpreter[ALG],
  bsonValidator: BsonValidatorInterpreter[ALG],
  bsonEncoder: BsonEncoderInterpreter[ALG],
  protobufValidator: ProtobufSequentialValidatorInterpreter[ALG],
  protobufEncoder: ProtobufSequentialEncoderInterpreter[ALG],
  //  customSwaggerInterpreter: CustomSwaggerInterpreter[ALG],
  pathStringToId: String => Either[StringToIdError, ID],
  charset: java.nio.charset.Charset = StandardCharsets.UTF_8
) {

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
      jsonValidator,
      jsonEncoder,
      bsonValidator,
      bsonEncoder,
      protobufValidator,
      protobufEncoder,
      charset
    )

}
