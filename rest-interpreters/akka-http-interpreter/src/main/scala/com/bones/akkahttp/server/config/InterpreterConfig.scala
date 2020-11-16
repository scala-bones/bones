package com.bones.akkahttp.server.config

import com.bones.sprayjson.SprayValidatorInterpreter
import com.bones.sprayjson.SprayEncoderInterpreter
import com.bones.swagger.SwaggerCoreInterpreter

case class InterpreterConfig[ALG[_], ID](
  jsonValidator: SprayValidatorInterpreter[ALG],
  jsonEncoder: SprayEncoderInterpreter[ALG],
  customSwaggerInterpreter: SwaggerCoreInterpreter[ALG],
  idDefinition: ALG[ID],
  charset: java.nio.charset.Charset
)
