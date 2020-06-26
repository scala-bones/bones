package com.bones.interpreter.custom

import com.bones.data.custom.{BigDecimalData, BooleanData, ByteArrayData, DoubleData, EnumerationData, FloatData, IntData, LongData, ScalaCoreValue, ShortData, StringData}
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder

trait ScalaCoreEncoder[OUT] extends InterchangeFormatEncoder[ScalaCoreValue, OUT] {
  val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  override def encode[A](alg: ScalaCoreValue[A]): A => OUT =
    alg match {
      case _: BooleanData => defaultEncoder.booleanToOut
      case _: StringData  => defaultEncoder.stringToOut
      case _: IntData     => defaultEncoder.intToOut
      case _: LongData    => defaultEncoder.longToOut
      case _: FloatData         => defaultEncoder.floatToOut
      case _: DoubleData        => defaultEncoder.doubleToOut
      case _: ShortData         => defaultEncoder.shortToOut
      case _: BigDecimalData    => defaultEncoder.bigDecimalToOut
      case _: ByteArrayData     => defaultEncoder.byteArrayToOut
      case e: EnumerationData[e, a] => {
        enum =>
          defaultEncoder.stringToOut(enum.toString)
      }

    }
}
