package com.bones.sprayjson.impl

import cats.data.NonEmptyList
import com.bones.Path
import com.bones.data.Error.{ExtractionErrors, RequiredValue, WrongTypeError}
import com.bones.data.ListData
import com.bones.interpreter.InterchangeFormatPrimitiveValidator
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsString, JsValue}

import scala.util.Try

object SprayPrimitiveValidator extends InterchangeFormatPrimitiveValidator[JsValue] {

  override def extractString[ALG2[_], A](
    op: ALG2[A],
    typeName: String
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], String] =
    in match {
      case JsString(value) => Right(value)
      case _               => Left(determineError(in, op, typeName, path))
    }

  override def extractInt[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], Int] =
    extractNumber(op, "Int", _.toInt)(in, path)

  override def extractFloat[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], Float] =
    extractNumber(op, "Float", _.toFloat)(in, path)

  private def extractNumber[N, ALG2[_], A](op: ALG2[A], expectedType: String, f: BigDecimal => N)(
    in: JsValue,
    path: List[String]
  ): Either[ExtractionErrors[String], N] = {
    in match {
      case JsNumber(value) =>
        Try(f(value)).toEither.left.map(ex =>
          NonEmptyList.one(WrongTypeError(path, expectedType, in.getClass.getSimpleName, Some(ex))))
      case _ => Left(determineError(in, op, expectedType, path))
    }

  }

  override def extractDouble[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], Double] =
    extractNumber(op, "Double", _.toDouble)(in, path)

  override def extractLong[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], Long] =
    extractNumber(op, "Long", _.toLong)(in, path)

  override def extractShort[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], Short] =
    extractNumber(op, "Short", _.toShort)(in, path)

  override def extractBool[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], Boolean] = {
    in match {
      case JsBoolean(bool) => Right(bool)
      case _               => Left(determineError(in, op, "Boolean", path))
    }
  }

  override def extractArray[ALG2[_], A](
    op: ListData[ALG2, A]
  )(in: JsValue, path: Path[String]): Either[ExtractionErrors[String], Seq[JsValue]] = {
    in match {
      case JsArray(elements) => Right(elements.toSeq)
      case _                 => Left(determineError(in, Left(op), op.typeNameOfT, path))
    }
  }

  override def extractBigDecimal[ALG2[_], A](
    op: ALG2[A]
  )(in: JsValue, path: List[String]): Either[ExtractionErrors[String], BigDecimal] =
    extractNumber(op, "BigDecimal", identity)(in, path)

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
    op: ALG2[A],
    typeName: String,
    path: List[String]
  ): ExtractionErrors[String] = {
    val error = in match {
      case JsNull => RequiredValue(path, typeName)
      case _      => WrongTypeError(path, typeName, in.getClass.getSimpleName, None)
    }
    NonEmptyList.one(error)
  }

}
