package com.bones.http4s.config

import com.bones.bson.{BsonEncoderInterpreter, BsonValidatorInterpreter}
import com.bones.circe.{CirceEncoderInterpreter, CirceValidatorInterpreter}
import com.bones.protobuf.{
  ProtobufSequentialEncoderInterpreter,
  ProtobufSequentialValidatorInterpreter
}
import com.bones.protobuf.messageType.ProtoFileGeneratorInterpreter
import com.bones.swagger.SwaggerCoreInterpreter

/**
  * Config objects for [[com.bones.http4s.ClassicCrudInterpreter]] and [[com.bones.http4s.RpcInterpreter]].
  *  Responsible for maintaining a collection of sub-interpreters so they don't have to be specified every time.
  * @param charset UTF_* is a good choice here.
  * @param idDefinition The id definition to be used (probably one representing a long, uuid or int)
  *
  */
case class InterpreterConfig[ALG[_], ID](
  jsonValidator: CirceValidatorInterpreter[ALG],
  jsonEncoder: CirceEncoderInterpreter[ALG],
  bsonValidator: BsonValidatorInterpreter[ALG],
  bsonEncoder: BsonEncoderInterpreter[ALG],
  protobufValidator: ProtobufSequentialValidatorInterpreter[ALG],
  protobufEncoder: ProtobufSequentialEncoderInterpreter[ALG],
  protobufFile: ProtoFileGeneratorInterpreter[ALG],
  customSwaggerInterpreter: SwaggerCoreInterpreter[ALG],
  idDefinition: ALG[ID],
  charset: java.nio.charset.Charset
)
