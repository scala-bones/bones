package com.bones.interpreter.encoder

/** Base trait defines the set of functions which need to be defined
  *  by a specific JSON library in order for the generic [[KvpInterchangeFormatEncoderInterpreter]].
  *  to encoder to be able to encode data to a specific library.
  *
  * @tparam OUT - One of the JSON types defined by a specific json library.
  */
trait InterchangeFormatPrimitiveEncoder[OUT] {

  def none: OUT

  def empty: OUT

  /** Combine two groups of values, for instance two JSON objects into a single JSON object */
  def combine(prefix: OUT, postfix: OUT): OUT

  /** Create a function which converts a boolean into the specific OUT type */
  def booleanToOut: Boolean => OUT

  /** Create a function which converts a String into the specific OUT type */
  def stringToOut: String => OUT

  /** Create a function which converts an Int into the specific OUT type */
  def intToOut: Int => OUT

  /** Create a function which converts a Long into the specific OUT type */
  def longToOut: Long => OUT

  def floatToOut: Float => OUT

  def doubleToOut: Double => OUT

  def shortToOut: Short => OUT

  def bigDecimalToOut: BigDecimal => OUT

  def byteArrayToOut: Array[Byte] => OUT

  def toOutList(list: List[OUT]): OUT

  def addStringField(element: OUT, name: String, value: String): OUT
}
