package com.bones.akkahttp.server

import java.nio.charset.StandardCharsets

import com.bones.sprayjson.values.{isoSprayEncoderInterpreter, isoSprayValidatorInterpreter}
import com.bones.swagger.values.defaultSwaggerInterpreter
import com.bones.syntax.{long, lv}

package object config {
  val longIdDefinition = long(lv.positive)

  val defaultLongConfig = InterpreterConfig(
    isoSprayValidatorInterpreter,
    isoSprayEncoderInterpreter,
    defaultSwaggerInterpreter,
    longIdDefinition,
    StandardCharsets.UTF_8
  )
}
