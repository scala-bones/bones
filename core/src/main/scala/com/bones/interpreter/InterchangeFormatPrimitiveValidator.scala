package com.bones.interpreter

import com.bones.Path
import com.bones.data.Error.RequiredValue
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil

trait InterchangeFormatPrimitiveValidator[IN] {

  /**
    * Override this to provide the ability to extract a String from the IN type.
    * @param typeName The resulting class we are tyring to extract.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  def extractString[ALG2[_], A](typeName: String): Validator[String, ALG2, String, IN]
  def extractInt[ALG2[_], A]: Validator[String, ALG2, Int, IN]
  def extractLong[ALG2[_], A]: Validator[String, ALG2, Long, IN]
  def extractBool[ALG2[_], A]: Validator[String, ALG2, Boolean, IN]
  def extractArray[ALG2[_], A](typeName: String): Validator[String, ALG2, Seq[IN], IN]
  def extractFloat[ALG2[_], A]: Validator[String, ALG2, Float, IN]
  def extractDouble[ALG2[_], A]: Validator[String, ALG2, Double, IN]
  def extractShort[ALG2[_], A]: Validator[String, ALG2, Short, IN]
  def extractBigDecimal[ALG2[_], A]: Validator[String, ALG2, BigDecimal, IN]
  def stringValue(in: IN, elementName: String): Option[String]

  def required[ALG2[_], A](
    typeName: String,
    validations: List[ValidationOp[A]],
    f: Validator[String, ALG2, A, IN],
  ): OptionalInputValidator[String, ALG2, A, IN] =
    (inOpt: Option[IN], path: Path[String]) =>
      for {
        json <- inOpt
          .toRight(List(RequiredValue(path, typeName)))
        a <- f.validateWithPath(json, path)
        _ <- ValidationUtil.validate(validations)(a, path)
      } yield a

}
