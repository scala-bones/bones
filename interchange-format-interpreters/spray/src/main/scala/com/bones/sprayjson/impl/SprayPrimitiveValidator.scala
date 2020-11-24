package com.bones.sprayjson.impl

import com.bones.Path
import com.bones.data.Error.{ExtractionErrors, RequiredValue, WrongTypeError}
import com.bones.interpreter.{InterchangeFormatPrimitiveValidator, Validator}
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsString, JsValue}

import scala.util.Try

object SprayPrimitiveValidator extends InterchangeFormatPrimitiveValidator[JsValue] {

  override def extractString[ALG2[_], A](
    typeName: String
  ): Validator[String, ALG2, String, JsValue] = { (in: JsValue, path: List[String]) =>
    in match {
      case JsString(value) => Right(value)
      case _               => Left(determineError(in, typeName, path))
    }
  }

  override def extractInt[ALG2[_], A]: Validator[String, ALG2, Int, JsValue] =
    extractNumber("Int", _.toInt)

  override def extractFloat[ALG2[_], A]: Validator[String, ALG2, Float, JsValue] =
    extractNumber("Float", _.toFloat)

  private def extractNumber[N, ALG2[_], A](expectedType: String, f: BigDecimal => N)(
    in: JsValue,
    path: List[String]
  ): Either[ExtractionErrors[String], N] = {
    in match {
      case JsNumber(value) =>
        Try(f(value)).toEither.left.map(ex =>
          List(WrongTypeError(path, expectedType, in.getClass.getSimpleName, Some(ex))))
      case _ => Left(determineError(in, expectedType, path))
    }

  }

  override def extractDouble[ALG2[_], A]: Validator[String, ALG2, Double, JsValue] =
    extractNumber("Double", _.toDouble)

  override def extractLong[ALG2[_], A]: Validator[String, ALG2, Long, JsValue] =
    extractNumber("Long", _.toLong)

  override def extractShort[ALG2[_], A]: Validator[String, ALG2, Short, JsValue] =
    extractNumber("Short", _.toShort)

  override def extractBool[ALG2[_], A]: Validator[String, ALG2, Boolean, JsValue] = {
    (in: JsValue, path: List[String]) =>
      in match {
        case JsBoolean(bool) => Right(bool)
        case _               => Left(determineError(in, "Boolean", path))
      }
  }

  override def extractArray[ALG2[_], A](
    typeName: String): Validator[String, ALG2, Seq[JsValue], JsValue] = {
    (in: JsValue, path: List[String]) =>
      in match {
        case JsArray(elements) => Right(elements)
        case _                 => Left(determineError(in, typeName, path))
      }
  }

  override def extractBigDecimal[ALG2[_], A]: Validator[String, ALG2, BigDecimal, JsValue] =
    extractNumber("BigDecimal", identity)

  override def stringValue(in: JsValue, elementName: String): Option[String] = {
    def asString(jsValue: JsValue): Option[String] = jsValue match {
      case JsString(value) => Some(value)
      case _               => None
    }

    for {
      obj <- Try(in.asJsObject).toOption
      element <- obj.fields.get(elementName)
      value <- asString(element)
    } yield value
  }

  protected def determineError[ALG2[_], A](
    in: JsValue,
    typeName: String,
    path: List[String]
  ): ExtractionErrors[String] = {
    val error = in match {
      case JsNull => RequiredValue(path, typeName)
      case _      => WrongTypeError(path, typeName, in.getClass.getSimpleName, None)
    }
    List(error)
  }

}
