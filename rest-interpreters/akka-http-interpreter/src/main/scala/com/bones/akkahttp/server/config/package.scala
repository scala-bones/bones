package com.bones.akkahttp.server

import com.bones.data.CommonValues.longIdDefinition

import java.nio.charset.StandardCharsets
import com.bones.sprayjson.values.{isoSprayEncoderInterpreter, isoSprayValidatorInterpreter}
import com.bones.swagger.values.defaultSwaggerInterpreter
import com.bones.syntax.{long, lv}

package object config {

  val defaultLongConfig = InterpreterConfig(
    isoSprayValidatorInterpreter,
    isoSprayEncoderInterpreter,
    defaultSwaggerInterpreter,
    longIdDefinition,
    StandardCharsets.UTF_8
  )
}
