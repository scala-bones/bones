package com.bones.circe

import com.bones.data.Error.{ExtractionErrors, RequiredValue, WrongTypeError}
import com.bones.interpreter.validator.{InterchangeFormatPrimitiveValidator, Validator}
import io.circe.Json

object CircePrimitiveValidator extends InterchangeFormatPrimitiveValidator[Json] {

  override def extractString[ALG2[_], A](typeName: String): Validator[String, ALG2, String, Json] =
    (in: Json, path: List[String]) => in.asString.toRight(determineError(in, typeName, path))

  override def extractInt[ALG2[_], A]: Validator[String, ALG2, Int, Json] =
    (in: Json, path: List[String]) =>
      in.asNumber
        .flatMap(_.toInt)
        .toRight(List(WrongTypeError(path, "Int", in.getClass.getSimpleName, None)))

  override def extractFloat[ALG2[_], A]: Validator[String, ALG2, Float, Json] =
    (in: Json, path: List[String]) =>
      in.asNumber
        .map(_.toDouble.toFloat)
        .toRight(List(WrongTypeError(path, "Float", in.getClass.getSimpleName, None)))

  override def extractDouble[ALG2[_], A]: Validator[String, ALG2, Double, Json] =
    (in: Json, path: List[String]) =>
      in.asNumber
        .map(_.toDouble)
        .toRight(List(WrongTypeError(path, "Double", in.getClass.getSimpleName, None)))

  override def extractLong[ALG2[_], A]: Validator[String, ALG2, Long, Json] =
    (in: Json, path: List[String]) =>
      in.asNumber
        .flatMap(_.toLong)
        .toRight(List(WrongTypeError(path, "Long", in.getClass.getSimpleName, None)))

  override def extractShort[ALG2[_], A]: Validator[String, ALG2, Short, Json] =
    (in: Json, path: List[String]) =>
      in.asNumber
        .flatMap(_.toShort)
        .toRight(List(WrongTypeError(path, "Short", in.getClass.getSimpleName, None)))

  override def extractBool[ALG2[_], A]: Validator[String, ALG2, Boolean, Json] =
    (in: Json, path: List[String]) => in.asBoolean.toRight(determineError(in, "Boolean", path))

  override def extractArray[ALG2[_], A](
    typeName: String): Validator[String, ALG2, Seq[Json], Json] =
    (in: Json, path: List[String]) =>
      in.asArray
        .toRight(determineError(in, typeName, path))

  override def extractBigDecimal[ALG2[_], A]: Validator[String, ALG2, BigDecimal, Json] =
    (in: Json, path: List[String]) =>
      in.asNumber
        .flatMap(_.toBigDecimal)
        .toRight(determineError(in, "BigDecimal", path))

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.asObject
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.asString
    } yield value

  protected def determineError[ALG2[_], A](
    in: Json,
    typeName: String,
    path: List[String]): ExtractionErrors[String] = {
    val error =
      if (in.isNull) RequiredValue(path, typeName)
      else WrongTypeError(path, typeName, in.getClass.getSimpleName, None)
    List(error)
  }

}
