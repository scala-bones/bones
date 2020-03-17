package com.bones.circe

import java.time.format.DateTimeFormatter

import com.bones.data.custom.{AllCustomAlgebras, JavaTimeValue}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator, JavaTimeEncoder, JavaTimeValidator}
import io.circe.Json

package object custom {

//  val allInterpreter: InterchangeFormatEncoder[AllCustomAlgebras, Json] =
//    CirceIsoJavaTimeEncoder ++ (CustomStringValidatorAndEncoder ++ CNilInterchangeFormatEncoder)

  object CirceIsoJavaTimeEncoder extends BaseCirceIsoJavaTimeEncoder {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter

    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter

  }

  trait BaseCirceIsoJavaTimeEncoder
    extends JavaTimeEncoder[Json]
      with JavaTimeValidator[Json] {

    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  }

  object CustomStringValidatorAndEncoder extends CustomStringEncoder[Json] with CustomStringValidator[Json] {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter

    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoCirceEncoderAndValidatorInterpreter
  }

}
