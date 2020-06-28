package com.bones.interpreter.values

import com.bones.data.values.CustomStringValue
import com.bones.interpreter.{InterchangeFormatEncoderValue, KvpInterchangeFormatEncoderInterpreter}

trait CustomStringEncoder[OUT] extends InterchangeFormatEncoderValue[CustomStringValue, OUT] {

  val baseEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  /**
    * Here we just delegate to the BaseEncoders means of serializing to a String.
    */
  override def encode[A](alg: CustomStringValue[A]): A => OUT =
    baseEncoder.stringToOut.asInstanceOf[A => OUT]
}
