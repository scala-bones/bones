package com.bones.interpreter

import cats.data.NonEmptyList
import com.bones.data.Error._
import com.bones.data.KeyDefinition.CoproductDataDefinition
import com.bones.data.template.KvpCollectionValidateAndDecode
import com.bones.data.values.CNilF
import com.bones.data.{
  KeyDefinition,
  KvpCoNil,
  KvpCollectionValue,
  KvpCoproduct,
  KvpCoproductCollectionHead,
  _
}
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.{ValidationUtil => vu}
import com.bones.{Path, Util}
import shapeless.ops.hlist.Prepend
import shapeless.{:+:, ::, CNil, Coproduct, HList, HNil, Inl, Inr, Nat}

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  * @tparam IN The base data type of the interchange format, such as io.circe.Json
  */
trait KvpInterchangeFormatValidatorInterpreter[ALG[_], IN]
    extends KvpCollectionValidateAndDecode[ALG, IN] {

  /** An additional string in the serialized format which states the coproduct type */
  val coproductTypeKey: String

  val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, IN]

  val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[IN]

  def generateValidator[A](
    kvpCollection: KvpCollection[ALG, A]): IN => Either[NonEmptyList[ExtractionError], A] = {
    fromKvpCollection(kvpCollection)(_, List.empty)

  }

  /**
    * Extend this to extract the value of type A from the input type IN
    * @param in The input type, for instance a base Json type.
    * @param kv The key and value definition describing this extraction.
    * @param headInterpreterF we will pass the appropriate extractXXX type based on type A
    * @param path The json path to the element such
    * @tparam A The type being extracted.
    * @return Either successful A or failure.  Should probably just return result from headInterpreterF.
    */
  protected def headValue[A](
    in: IN,
    kv: KeyDefinition[ALG, A],
    headInterpreterF: (Option[IN], Path) => Either[NonEmptyList[ExtractionError], A],
    path: Path): Either[NonEmptyList[ExtractionError], A]

  def invalidValue[T](
    in: IN,
    typeName: String,
    path: Path): Left[NonEmptyList[ExtractionError], Nothing]

  override def coproductType(in: IN): Option[CoproductType] =
    interchangeFormatPrimitiveValidator.stringValue(in, coproductTypeKey)

  override type CoproductType = String

  protected def isEmpty(json: IN): Boolean

  override def keyDefinition[A](value: KeyDefinition[ALG, A])
    : (IN, List[String]) => Either[NonEmptyList[ExtractionError], A] = { (in, path) =>
    {
      headValue(in, value, determineValidator(value.dataDefinition), path)
    }
  }

  protected def determineValidator[A](
    value: Either[HigherOrderValue[ALG, A], ALG[A]]
  ): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] = {
    value match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(ext) => interchangeFormatValidator.validate(ext)
    }
  }

  protected def valueDefinition[A](fgo: HigherOrderValue[ALG, A])
    : (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] = {
    val result: (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
      fgo match {
        case op: OptionalValue[ALG, a] @unchecked =>
          val applied = determineValidator(op.valueDefinitionOp)
          (in: Option[IN], path: Path) =>
            in match {
              case None                  => Right(None)
              case Some(n) if isEmpty(n) => Right(None)
              case some @ Some(json)     => applied(some, path).map(Some(_))
            }

        case ed: EitherData[ALG, a, b] @unchecked =>
          val optionalA = determineValidator(ed.definitionA)
          val optionalB = determineValidator(ed.definitionB)
          (in: Option[IN], path: Path) =>
            {
              optionalA(in, path) match {
                case Left(err1) =>
                  optionalB(in, path) match {
                    case Right(b) => Right(Right(b))
                    case Left(err2) => {
                      val errors = RequiredValue(path, ed.typeNameOfA) :: err2 ::: err1
                      Left(errors)
                    }
                  }
                case Right(a) => Right(Left(a))
              }
            }
        case op: ListData[ALG, t] @unchecked =>
          val valueF = determineValidator(op.tDefinition)

          def appendArrayIndex(path: Path, index: Int): List[String] = {
            if (path.isEmpty) path
            else
              path.updated(path.length - 1, path(path.length - 1) + s"[${index}]")
          }

          def traverseArray(
            arr: Seq[IN],
            path: Path): Either[NonEmptyList[ExtractionError], List[t]] = {
            val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], t]] =
              arr.zipWithIndex.map(jValue =>
                valueF(Some(jValue._1), appendArrayIndex(path, jValue._2)))

            arrayApplied
              .foldLeft[Either[NonEmptyList[ExtractionError], List[t]]](Right(List.empty))((b, v) =>
                (b, v) match {
                  case (Right(a), Right(i)) => Right(a :+ i)
                  case (Left(a), Left(b))   => Left(a ::: b)
                  case (Left(x), _)         => Left(x)
                  case (_, Left(x))         => Left(x)
              })
          }

          (inOpt: Option[IN], path: Path) =>
            {
              for {
                in <- inOpt.toRight(NonEmptyList.one(RequiredValue(path, op.typeNameOfT)))
                arr <- interchangeFormatPrimitiveValidator.extractArray(op)(in, path)
                listOfIn <- traverseArray(arr, path)
              } yield listOfIn
            }
        case op: KvpCollectionValue[ALG, A] @unchecked => {
          val fg: (IN, List[String]) => Either[NonEmptyList[ExtractionError], A] =
            fromKvpCollection(op.kvpCollection)
          (jsonOpt: Option[IN], path: Path) =>
            {
              jsonOpt match {
                case Some(json) =>
                  fg(json, path)
                    .flatMap(res => vu.validate[A](op.validations)(res, path))
                case None => Left(NonEmptyList.one(RequiredValue(path, op.typeName)))
              }
            }
        }
      }
    result
  }

  private def combine[H <: HList, T <: HList, HO <: HList](
    path: List[String],
    head: Either[NonEmptyList[Error.ExtractionError], H],
    tail: Either[NonEmptyList[Error.ExtractionError], T],
    prepend: Prepend.Aux[H, T, HO],
    validations: List[ValidationOp[HO]]) = {
    Util
      .eitherMap2(head, tail)((l1: H, l2: T) => {
        prepend.apply(l1, l2)
      })
      .flatMap { l =>
        vu.validate[HO](validations)(l, path)
      }
  }
}
