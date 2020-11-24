package com.bones.interpreter.values

import com.bones.data.values.CustomStringValue
import com.bones.interpreter.{
  Encoder,
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder
}

trait CustomStringEncoder[OUT] extends InterchangeFormatEncoderValue[CustomStringValue, OUT] {

  val baseEncoder: InterchangeFormatPrimitiveEncoder[OUT]

  /**
    * Here we just delegate to the BaseEncoders means of serializing to a String.
    */
  override def createEncoder[A](alg: CustomStringValue[A]): Encoder[CustomStringValue, A, OUT] = {
    (a: A) =>
      baseEncoder.stringToOut(a.asInstanceOf[String])
  }
}
