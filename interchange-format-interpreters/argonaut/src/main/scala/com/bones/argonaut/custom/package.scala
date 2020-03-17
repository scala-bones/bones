package com.bones.argonaut

import java.time.format.DateTimeFormatter

import argonaut.Json
import com.bones.data.custom.{AllCustomAlgebras, CNilF, CustomStringValue}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder.CNilInterchangeFormatEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator.CNilInterchangeFormatValidator
import com.bones.interpreter.custom.{CustomStringEncoder, CustomStringValidator, JavaTimeEncoder, JavaTimeValidator}
import com.bones.interpreter.{KvpInterchangeFormatEncoderInterpreter, KvpInterchangeFormatValidatorInterpreter}
import shapeless.:+:

package object custom {


  type CustomStringCoproduct[A] = CustomStringValue[A] :+: CNilF[A]
  val someCustomEncoder: InterchangeFormatEncoder[CustomStringCoproduct, Json] =
    CustomStringEncoder ++ CNilInterchangeFormatEncoder[Json]()
  val someCustomValidator: InterchangeFormatValidator[CustomStringCoproduct, Json] =
    CustomStringValidator ++ CNilInterchangeFormatValidator[Json]()



  // The goal is to have all encoders use the following code, but I'm getting type errors, so
  // Instead we will build the interpreters up one step at a time.
  // ArgonautIsoJavaTimeEncoder ++ (CustomStringEncoder ++ CNilInterchangeFormatEncoder[Json]())
  val allEncoders: InterchangeFormatEncoder[AllCustomAlgebras, Json] =
  //   ArgonautIsoJavaTimeEncoder ++ (CustomStringEncoder ++ CNilInterchangeFormatEncoder[Json]())
  ArgonautIsoJavaTimeEncoder ++ someCustomEncoder

  val allValidators: InterchangeFormatValidator[AllCustomAlgebras, Json] =
    ArgonautIsoJavaTimeValidator ++ someCustomValidator



  object ArgonautIsoJavaTimeValidator extends BaseArgonautIsoJavaTimeValidator {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object ArgonautIsoJavaTimeEncoder extends BaseArgonautIsoJavaTimeEncoder {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  trait BaseArgonautIsoJavaTimeValidator extends JavaTimeValidator[Json] {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  }

  /** Encoder/Validator which uses default ISO format. */
  trait BaseArgonautIsoJavaTimeEncoder
    extends JavaTimeEncoder[Json]  {
    override val instantFormatter: DateTimeFormatter = DateTimeFormatter.ISO_INSTANT
    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
  }

  object CustomStringValidator extends CustomStringValidator[Json] {
    override val baseValidator: KvpInterchangeFormatValidatorInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }

  object CustomStringEncoder extends CustomStringEncoder[Json] {
    override val baseEncoder: KvpInterchangeFormatEncoderInterpreter[Json] =
      IsoArgonautEncoderAndValidatorInterpreter
  }


}
