package com.bones.json4s

import com.bones.interpreter.deltavalidator.{
  InterchangeFormatDeltaValidatorValue,
  KvpInterchangeFormatDeltaValidatorInterpreter,
  PrimitiveInterchangeFormat
}
import com.bones.json4s.impl.Json4SPrimitiveDeltaValidatorValue
import org.json4s.JValue

trait Json4sDeltaValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatDeltaValidatorInterpreter[ALG, JValue] {}
