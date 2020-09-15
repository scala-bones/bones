package com.bones.interpreter

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, RequiredValue}
import com.bones.data.ListData
import com.bones.Path
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.ValidationUtil

trait InterchangeFormatPrimitiveValidator[IN] {

  /**
    * Override this to provide the ability to extract a String from the IN type.
    * @param typeName The resulting class we are tyring to extract.
    * @param in The interchange format input type.
    * @param path The path hierarchy.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  def extractString[ALG2[_], A](dataDefinition: ALG2[A], typeName: String)(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], String]
  def extractInt[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Int]
  def extractLong[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Long]
  def extractBool[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Boolean]
  def extractArray[ALG2[_], A](
    op: ListData[ALG2, A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractFloat[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Float]
  def extractDouble[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Double]
  def extractShort[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Short]
  def extractBigDecimal[ALG2[_], A](
    dataDefinition: ALG2[A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], BigDecimal]
  def stringValue(in: IN, elementName: String): Option[String]

  def required[ALG2[_], A](
    coproductDataDefinition: CoproductDataDefinition[ALG2, A],
    typeName: String,
    validations: List[ValidationOp[A]],
    f: (IN, List[String]) => Either[NonEmptyList[ExtractionError], A],
  ): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
    (inOpt: Option[IN], path: Path) =>
      for {
        json <- inOpt
          .toRight(NonEmptyList.one(RequiredValue(path, typeName)))
        a <- f(json, path)
        _ <- ValidationUtil.validate(validations)(a, path)
      } yield a

}
