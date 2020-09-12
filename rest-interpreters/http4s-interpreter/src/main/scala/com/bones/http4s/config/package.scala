package com.bones.http4s

import java.nio.charset.StandardCharsets

import com.bones.data.values.DefaultValues
import com.bones.syntax._

package object config {

  val longIdDefinition = long(lv.positive)
  val intIdDefinition = int(iv.positive)

  val defaultLong = InterpreterConfig[DefaultValues, Long](
    com.bones.circe.values.isoCirceValidatorInterpreter,
    com.bones.circe.values.isoCirceEncoderInterpreter,
    com.bones.bson.values.defaultBsonValidatorInterpreter,
    com.bones.bson.values.defaultBsonEncoderInterpreter,
    com.bones.protobuf.values.defaultUtcValidator,
    com.bones.protobuf.values.defaultEncoder,
    com.bones.protobuf.messageType.defaultProtoFile,
    com.bones.swagger.values.defaultSwaggerInterpreter,
    longIdDefinition,
    StandardCharsets.UTF_8
  )

  val defaultInt = InterpreterConfig[DefaultValues, Int](
    com.bones.circe.values.isoCirceValidatorInterpreter,
    com.bones.circe.values.isoCirceEncoderInterpreter,
    com.bones.bson.values.defaultBsonValidatorInterpreter,
    com.bones.bson.values.defaultBsonEncoderInterpreter,
    com.bones.protobuf.values.defaultUtcValidator,
    com.bones.protobuf.values.defaultEncoder,
    com.bones.protobuf.messageType.defaultProtoFile,
    com.bones.swagger.values.defaultSwaggerInterpreter,
    intIdDefinition,
    StandardCharsets.UTF_8
  )
}
