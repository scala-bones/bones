package com.bones.interpreter.custom

import com.bones.data.custom.CustomStringValue
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder

trait CustomStringEncoder[OUT] extends InterchangeFormatEncoder[CustomStringValue, OUT] {

  val baseEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  /**
    * Here we just delegate to the BaseEncoders means of serializing to a String.
    */
  override def encode[A](alg: CustomStringValue[A]): A => OUT =
    baseEncoder.stringToOut.asInstanceOf[A => OUT]
}
