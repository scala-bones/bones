package com.bones.interpreter.values

import com.bones.data.values._
import com.bones.interpreter.encoder.{
  Encoder,
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder
}

trait ScalaCoreEncoder[OUT] extends InterchangeFormatEncoderValue[ScalaCoreValue, OUT] {
  val defaultEncoder: InterchangeFormatPrimitiveEncoder[OUT]

  override def createEncoder[A](alg: ScalaCoreValue[A]): Encoder[ScalaCoreValue, A, OUT] = {
    val result: A => OUT = alg match {
      case _: BooleanData    => defaultEncoder.booleanToOut
      case _: StringData     => defaultEncoder.stringToOut
      case _: IntData        => defaultEncoder.intToOut
      case _: LongData       => defaultEncoder.longToOut
      case _: FloatData      => defaultEncoder.floatToOut
      case _: DoubleData     => defaultEncoder.doubleToOut
      case _: ShortData      => defaultEncoder.shortToOut
      case _: BigDecimalData => defaultEncoder.bigDecimalToOut
      case _: ByteArrayData  => defaultEncoder.byteArrayToOut
      case e: EnumerationData[e, a] => { enum =>
        defaultEncoder.stringToOut(enum.toString)
      }
    }
    new Encoder[ScalaCoreValue, A, OUT] {
      override def encode(a: A): OUT = result(a)
    }
  }
}
