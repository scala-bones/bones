package com.bones.interpreter.values

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  KvpInterchangeFormatEncoderInterpreter
}

trait JavaUtilEncoder[OUT] extends InterchangeFormatEncoderValue[JavaUtilValue, OUT] {

  val defaultEncoder: InterchangeFormatPrimitiveEncoder[OUT]

  override def encode[A](alg: JavaUtilValue[A]): A => OUT = alg match {
    case UuidData(_) =>
      (uuid: UUID) =>
        defaultEncoder.stringToOut.apply(uuid.toString)

  }
}
