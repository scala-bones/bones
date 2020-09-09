package com.bones.interpreter

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
