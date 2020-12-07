package com.bones.json4s.impl

import com.bones.Path
import com.bones.Util.OmittedValue
import com.bones.data.Error.{ExtractionErrors, WrongTypeError}
import com.bones.interpreter.deltavalidator.{DeltaValueValidator, PrimitiveInterchangeFormat}
import org.json4s.JsonAST.{JNull, JValue}
import org.json4s.{JNothing, JObject}

object Json4SPrimitiveDeltaValidatorValueCanBeOmitted
    extends PrimitiveInterchangeFormat[JValue, String] {

  /** Override this to provide the ability to extract a String from the IN type.
    *
    * @param typeName The resulting class we are tyring to extract.
    * @return The extracted String or an Error
    */
  override def extractString[ALG[_]](
    typeName: String): DeltaValueValidator[String, ALG, String, JValue] =
    extract[ALG, String](
      "String",
      Json4sPrimitiveValidator
        .extractString("String")
        .validateWithPath(_, _))

  override def extractInt[ALG[_]]: DeltaValueValidator[String, ALG, Int, JValue] =
    extract("Int", Json4sPrimitiveValidator.extractInt.validateWithPath)

  override def extractLong[ALG[_]]: DeltaValueValidator[String, ALG, Long, JValue] =
    extract("Int", Json4sPrimitiveValidator.extractLong.validateWithPath)

  override def extractBool[ALG[_]]: DeltaValueValidator[String, ALG, Boolean, JValue] =
    extract("Boolean", Json4sPrimitiveValidator.extractBool.validateWithPath)

  override def extractFloat[ALG[_]]: DeltaValueValidator[String, ALG, Float, JValue] =
    extract("Float", Json4sPrimitiveValidator.extractFloat.validateWithPath)

  override def extractDouble[ALG[_]]: DeltaValueValidator[String, ALG, Double, JValue] =
    extract("Double", Json4sPrimitiveValidator.extractDouble.validateWithPath)

  override def extractShort[ALG[_]]: DeltaValueValidator[String, ALG, Short, JValue] =
    extract("Short", Json4sPrimitiveValidator.extractShort.validateWithPath)

  override def extractBigDecimal[ALG[_]]: DeltaValueValidator[String, ALG, BigDecimal, JValue] =
    extract("BigDecimal", Json4sPrimitiveValidator.extractBigDecimal.validateWithPath)

  override def extractArray[ALG[_]](
    typeName: String): DeltaValueValidator[String, ALG, Seq[JValue], JValue] =
    extract("Array", Json4sPrimitiveValidator.extractArray(typeName).validateWithPath)

  private def extractObjectF(
    json: JValue,
    path: Path[String]): Either[ExtractionErrors[String], JValue] =
    json match {
      case j: JObject => Right(j)
      case _          => Left(List(WrongTypeError(path, "BigDecimal", json.getClass.getSimpleName, None)))
    }

  override def extractObject[ALG[_]]: DeltaValueValidator[String, ALG, JValue, JValue] =
    extract("Object", extractObjectF)

  override def stringValue(in: JValue, elementName: String): Option[String] =
    Json4sPrimitiveValidator.stringValue(in, elementName)

  private val nullList = List(JNothing, JNull)

  private def extract[ALG[_], A](
    typeName: String,
    converter: (JValue, Path[String]) => Either[ExtractionErrors[String], A])
    : DeltaValueValidator[String, ALG, A, JValue] =
    (input, key, path) => {
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
