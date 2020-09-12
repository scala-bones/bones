package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.effect.Sync
import com.bones.data.KvpCollection
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.http4s.config.InterpreterConfig
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

class RpcInterpreter[ALG[_], ID: Manifest](
  interpreters: InterpreterConfig[ALG, ID],
  path: String,
  pathStringToId: String => Either[StringToIdError, ID]
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
      interpreters.jsonValidator,
      interpreters.jsonEncoder,
      interpreters.bsonValidator,
      interpreters.bsonEncoder,
      interpreters.protobufValidator,
      interpreters.protobufEncoder,
      interpreters.charset
    )

}
