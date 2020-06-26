package com.bones.interpreter.custom

import java.util.UUID

import com.bones.data.custom.{JavaUtilValue, UuidData}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder

trait JavaUtilEncoder[OUT] extends InterchangeFormatEncoder[JavaUtilValue, OUT] {

  val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  override def encode[A](alg: JavaUtilValue[A]): A => OUT = alg match {
    case UuidData(_) =>
      (uuid: UUID) =>
        defaultEncoder.stringToOut.apply(uuid.toString)

  }
}
