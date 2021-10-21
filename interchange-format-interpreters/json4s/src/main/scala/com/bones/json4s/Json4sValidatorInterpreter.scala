package com.bones.json4s

import com.bones.data.Error.{ExtractionErrors, WrongTypeError}
import com.bones.data.KeyDefinition
import com.bones.interpreter.validator.{
  InterchangeFormatPrimitiveValidator,
  InterchangeFormatValidatorValue,
  KvpInterchangeFormatValidatorInterpreter,
  OptionalInputValidator
}
import org.json4s.JsonAST._
import org.json4s.{JNull, JObject}

trait Json4sValidatorInterpreter[ALG[_]]
    extends KvpInterchangeFormatValidatorInterpreter[ALG, JValue] {

  val coproductTypeKey: String
  val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, JValue]
  val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[JValue]

  override def isEmpty(value: JValue): Boolean = value match {
    case JNull => true
    case _     => false
  }
  override def invalidValue[T](
    value: JValue,
    typeName: String,
    path: List[String]
  ): Left[List[WrongTypeError[String, T]], Nothing] = {
    val invalid = value match {
      case JNull       => classOf[Nothing]
      case JArray(_)   => classOf[Array[_]]
      case JBool(_)    => classOf[Boolean]
      case JInt(_)     => classOf[Int]
      case JObject(_)  => classOf[Object]
      case JString(_)  => classOf[String]
      case JDecimal(_) => classOf[BigDecimal]
      case JDouble(_)  => classOf[Double]
      case JLong(_)    => classOf[Long]
      case JNothing    => classOf[Nothing]
      case JSet(_)     => classOf[Set[JValue]]
    }

    Left(List(WrongTypeError(path, typeName, invalid.getSimpleName, None)))
  }

  override def headValue[A](
    in: JValue,
    kv: KeyDefinition[String, ALG, A],
    headInterpreter: OptionalInputValidator[String, ALG, A, JValue],
    path: List[String]
  ): Either[ExtractionErrors[String], A] =
    in match {
      case JObject(obj) =>
        headInterpreter.validateWithPath(obj.find(_._1 == kv.key).map(_._2), path)
      case _ => sys.error(s"Expected JObject, received: ${in}")
    }

}
