package com.bones.interpreter.deltavalidator

import com.bones.Path
import com.bones.Util.CanBeOmitted
import com.bones.data.Error.ExtractionErrors

trait PrimitiveInterchangeFormat[IN, K] {

  /** Override this to provide the ability to extract a String from the IN type.
    *
    * @param typeName
    *   The resulting class we are tyring to extract.
    * @tparam A
    *   The expected resulting type, eg String or Enumerated Type which we are trying to extract
    *   from a string.
    * @return
    *   The extracted String or an Error
    */
  def extractString[ALG[_]](typeName: String): DeltaValueValidator[K, ALG, String, IN]

  def extractInt[ALG[_]]: DeltaValueValidator[K, ALG, Int, IN]

  def extractLong[ALG[_]]: DeltaValueValidator[K, ALG, Long, IN]

  def extractBool[ALG[_]]: DeltaValueValidator[K, ALG, Boolean, IN]

  def extractArray[ALG[_]](typeName: String): DeltaValueValidator[K, ALG, Seq[IN], IN]

  def extractFloat[ALG[_]]: DeltaValueValidator[K, ALG, Float, IN]

  def extractDouble[ALG[_]]: DeltaValueValidator[K, ALG, Double, IN]

  def extractShort[ALG[_]]: DeltaValueValidator[K, ALG, Short, IN]

  def extractBigDecimal[ALG[_]]: DeltaValueValidator[K, ALG, BigDecimal, IN]

  def extractObject[ALG[_]]: DeltaValueValidator[K, ALG, IN, IN]

  def stringValue(in: IN, elementName: String): Option[String]
}
