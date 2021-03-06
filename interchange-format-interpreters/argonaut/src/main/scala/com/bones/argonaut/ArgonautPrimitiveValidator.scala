package com.bones.argonaut

import argonaut.Argonaut.JsonBoolean
import argonaut.Json
import com.bones.data.Error.{ExtractionErrors, WrongTypeError}
import com.bones.interpreter.validator.{InterchangeFormatPrimitiveValidator, Validator}

object ArgonautPrimitiveValidator extends InterchangeFormatPrimitiveValidator[Json] {
  override def extractString[ALG[_], A](typeName: String): Validator[String, ALG, String, Json] = {
    (in: Json, path: List[String]) =>
      in.string.toRight(List(WrongTypeError(path, typeName, in.getClass.getSimpleName, None)))
  }

  override def extractShort[ALG[_], A]: Validator[String, ALG, Short, Json] =
    (in: Json, path: List[String]) =>
      in.number
        .flatMap(n => n.toShort)
        .toRight(List(WrongTypeError(path, "Short", in.getClass.getSimpleName, None)))

  override def extractInt[ALG[_], A]: Validator[String, ALG, Int, Json] =
    (in: Json, path: List[String]) =>
      in.number
        .flatMap(n => n.toInt)
        .toRight(List(WrongTypeError(path, "Int", in.getClass.getSimpleName, None)))

  override def extractLong[ALG[_], A]: Validator[String, ALG, Long, Json] =
    (in: Json, path: List[String]) =>
      in.number
        .flatMap(_.toLong)
        .toRight(List(WrongTypeError(path, "Long", in.getClass.getSimpleName, None)))

  override def extractBool[ALG[_], A]: Validator[String, ALG, Boolean, Json] =
    (in: Json, path: List[String]) =>
      in.bool.toRight(List(WrongTypeError(path, "Boolean", in.getClass.getSimpleName, None)))

  override def extractArray[ALG[_], A](typeName: String): Validator[String, ALG, Seq[Json], Json] =
    (in: Json, path: List[String]) =>
      in.array.toRight(List(WrongTypeError(path, typeName, in.getClass.getSimpleName, None)))

  override def extractFloat[ALG[_], A]: Validator[String, ALG, Float, Json] =
    (in: Json, path: List[String]) =>
      in.number
        .flatMap(n => n.toFloat)
        .toRight(List(WrongTypeError(path, "Float", in.getClass.getSimpleName, None)))

  override def extractDouble[ALG[_], A]: Validator[String, ALG, Double, Json] =
    (in: Json, path: List[String]) =>
      in.number
        .flatMap(n => n.toDouble)
        .toRight(List(WrongTypeError(path, "Double", in.getClass.getSimpleName, None)))

  override def extractBigDecimal[ALG[_], A]: Validator[String, ALG, BigDecimal, Json] =
    (in: Json, path: List[String]) =>
      in.number
        .map(_.toBigDecimal)
        .toRight(List(WrongTypeError(path, "BigDecimal", in.getClass.getSimpleName, None)))

  override def stringValue(in: Json, elementName: String): Option[String] =
    for {
      obj <- in.obj
      element <- obj.toList.find(_._1 == elementName)
      value <- element._2.string
    } yield value

}
