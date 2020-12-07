package com.bones.interpreter.deltavalidator

import com.bones.Path
import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors

trait InterchangeFormatValidatorValueCanBeOmitted[IN, K] {

  /** Override this to provide the ability to extract a String from the IN type.
    *
    * @param typeName The resulting class we are tyring to extract.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  def extractString[ALG2[_], A](
    typeName: String): (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, String]]

  def extractInt[ALG2[_], A]: (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Int]]

  def extractLong[ALG2[_], A]
    : (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Long]]

  def extractBool[ALG2[_], A]
    : (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Boolean]]

  def extractArray[ALG2[_], A](
    typeName: String): (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Seq[IN]]]

  def extractFloat[ALG2[_], A]
    : (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Float]]

  def extractDouble[ALG2[_], A]
    : (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Double]]

  def extractShort[ALG2[_], A]
    : (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, Short]]

  def extractBigDecimal[ALG2[_], A]
    : (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, BigDecimal]]

  def extractObject[ALG2[_]]: (K, Path[K], IN) => Either[ExtractionErrors[K], CanBeOmitted[K, IN]]

  def stringValue(in: IN, elementName: String): Option[String]
}
