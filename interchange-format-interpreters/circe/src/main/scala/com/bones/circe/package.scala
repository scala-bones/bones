package com.bones

import java.time.format.DateTimeFormatter

import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.{DateToStringEncoder, NoAlgebraEncoder}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.{NoAlgebraValidator, StringToDateValidator}
import io.circe.Json

package object circe {

  object IsoCirceEncoderAndValidatorInterpreter
      extends CirceEncoderInterpreter
      with CirceValidatorInterpreter
      with DateToStringEncoder[Json]
      with StringToDateValidator[Json] {

    override val coproductTypeKey: String = "type"

    override def localDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    override def localDateFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE
    override def localTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_TIME
  }

  val noAlgebraEncoder = NoAlgebraEncoder[Json]
  val noAlgebraValidator = NoAlgebraValidator[Json]


}
