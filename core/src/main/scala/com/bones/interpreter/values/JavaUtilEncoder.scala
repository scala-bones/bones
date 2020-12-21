package com.bones.interpreter.values

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.interpreter.encoder.{
  Encoder,
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder
}

trait JavaUtilEncoder[OUT] extends InterchangeFormatEncoderValue[JavaUtilValue, OUT] {

  val defaultEncoder: InterchangeFormatPrimitiveEncoder[OUT]

  override def generateEncoder[A](alg: JavaUtilValue[A]): Encoder[JavaUtilValue, A, OUT] =
    alg match {
      case UuidData(_) =>
        (uuid: UUID) =>
          defaultEncoder.stringToOut.apply(uuid.toString)

    }
}
