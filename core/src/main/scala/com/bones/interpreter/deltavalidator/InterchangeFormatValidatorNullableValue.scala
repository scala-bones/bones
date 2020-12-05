package com.bones.interpreter.deltavalidator

import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors

trait InterchangeFormatValidatorNullableValue[IN, K] {

  /** Override this to provide the ability to extract a String from the IN type.
    *
    * @param typeName The resulting class we are tyring to extract.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  def extractString[ALG2[_], A](
    typeName: String): K => Either[ExtractionErrors[K], NullableResult[K, String]]

  def extractInt[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, Int]]

  def extractLong[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, Long]]

  def extractBool[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, Boolean]]

  def extractArray[ALG2[_], A](
    typeName: String): K => Either[ExtractionErrors[K], NullableResult[K, Seq[IN]]]

  def extractFloat[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, Float]]

  def extractDouble[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, Double]]

  def extractShort[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, Short]]

  def extractBigDecimal[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, BigDecimal]]

  def extractObject[ALG2[_], A]: K => Either[ExtractionErrors[K], NullableResult[K, IN]]

  def stringValue(in: IN, elementName: String): Option[String]
}
