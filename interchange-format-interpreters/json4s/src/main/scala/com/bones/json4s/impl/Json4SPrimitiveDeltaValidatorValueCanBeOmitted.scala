package com.bones.json4s.impl

import com.bones.Path
import com.bones.Util.{OmittedValue, CanBeOmitted}
import com.bones.data.Error.{ExtractionErrors, WrongTypeError}
import com.bones.interpreter.deltavalidator.InterchangeFormatValidatorValueCanBeOmitted
import org.json4s.{JNothing, JObject}
import org.json4s.JsonAST.{JNull, JValue}

object Json4SPrimitiveDeltaValidatorValueCanBeOmitted
    extends InterchangeFormatValidatorValueCanBeOmitted[JValue, String] {

  /** Override this to provide the ability to extract a String from the IN type.
    *
    * @param typeName The resulting class we are tyring to extract.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  override def extractString[ALG2[_], A](typeName: String): (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, String]] =
    extract(
      "String",
      Json4sPrimitiveValidator
        .extractString("String")
        .validateWithPath(_, _))

  override def extractInt[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Int]] =
    extract("Int", Json4sPrimitiveValidator.extractInt.validateWithPath)

  override def extractLong[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Long]] =
    extract("Int", Json4sPrimitiveValidator.extractLong.validateWithPath)

  override def extractBool[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Boolean]] =
    extract("Boolean", Json4sPrimitiveValidator.extractBool.validateWithPath)

  override def extractFloat[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Float]] =
    extract("Float", Json4sPrimitiveValidator.extractFloat.validateWithPath)

  override def extractDouble[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Double]] =
    extract("Double", Json4sPrimitiveValidator.extractDouble.validateWithPath)

  override def extractShort[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Short]] =
    extract("Short", Json4sPrimitiveValidator.extractShort.validateWithPath)

  override def extractBigDecimal[ALG2[_], A]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, BigDecimal]] =
    extract("BigDecimal", Json4sPrimitiveValidator.extractBigDecimal.validateWithPath)

  override def extractArray[ALG2[_], A](typeName: String): (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, Seq[JValue]]] =
    extract("Array", Json4sPrimitiveValidator.extractArray(typeName).validateWithPath)

  private def extractObject(
    json: JValue,
    path: Path[String]): Either[ExtractionErrors[String], JValue] =
    json match {
      case j: JObject => Right(j)
      case _          => Left(List(WrongTypeError(path, "BigDecimal", json.getClass.getSimpleName, None)))
    }

  override def extractObject[ALG2[_]]: (
    String,
    Path[String],
    JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, JValue]] =
    extract("Object", extractObject)

  override def stringValue(in: JValue, elementName: String): Option[String] =
    Json4sPrimitiveValidator.stringValue(in, elementName)

  private val nullList = List(JNothing, JNull)
  type Key = String
  private def extract[A](
    typeName: String,
    converter: (JValue, Path[String]) => Either[ExtractionErrors[String], A])
    : (Key, Path[String], JValue) => Either[ExtractionErrors[String], CanBeOmitted[String, A]] =
    (key, path, input) => {
      input.findField(_._1 == key).map(_._2) match {
        case None => Right(Left(List(OmittedValue(key, typeName, path :+ key))))
        case Some(jValue) if nullList.contains(jValue) =>
          Right(Left(List(OmittedValue(key, typeName, List(key)))))
        case Some(jValue) =>
          converter(jValue, path :+ key)
            .map(Right(_))
      }
    }
}
