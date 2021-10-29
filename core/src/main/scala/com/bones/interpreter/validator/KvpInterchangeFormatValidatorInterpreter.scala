package com.bones.interpreter.validator

import com.bones.data.Error._
import com.bones.data.template.KvpCollectionValidateAndDecode
import com.bones.data.{KeyDefinition, KvpCollectionValue, _}
import com.bones.validation.{ValidationUtil => vu}

trait OptionalInputValidator[K, ALG[_], A, IN] { self =>
  def validate(in: Option[IN]): Either[ExtractionErrors[K], A] = validateWithPath(in, List.empty)
  def validateWithPath(in: Option[IN], path: List[K]): Either[ExtractionErrors[K], A]

  def map[B](f: A => B): OptionalInputValidator[K, ALG, B, IN] =
    (in: Option[IN], path: List[K]) => self.validateWithPath(in, path).map(f)

}

/** Base trait for converting from an interchange format such as JSON to an HList or Case class.
  * @tparam IN
  *   The base data type of the interchange format, such as io.circe.Json
  */
trait KvpInterchangeFormatValidatorInterpreter[ALG[_], IN]
    extends KvpCollectionValidateAndDecode[String, ALG, IN] {

  /** An additional string in the serialized format which states the coproduct type */
  val coproductTypeKey: String

  val interchangeFormatValidator: InterchangeFormatValidatorValue[ALG, IN]

  val interchangeFormatPrimitiveValidator: InterchangeFormatPrimitiveValidator[IN]

  def generateValidator[A](
    kvpCollection: KvpCollection[String, ALG, A]
  ): Validator[String, ALG, A, IN] = {
    fromKvpCollection(kvpCollection)
  }

  /** Extend this to extract the value of type A from the input type IN
    * @param in
    *   The input type, for instance a base Json type.
    * @param kv
    *   The key and value definition describing this extraction.
    * @param headInterpreterF
    *   we will pass the appropriate extractXXX type based on type A
    * @param path
    *   The json path to the element such
    * @tparam A
    *   The type being extracted.
    * @return
    *   Either successful A or failure. Should probably just return result from headInterpreterF.
    */
  protected def headValue[A](
    in: IN,
    kv: KeyDefinition[String, ALG, A],
    headInterpreterF: OptionalInputValidator[String, ALG, A, IN],
    path: Path[String]
  ): Either[ExtractionErrors[String], A]

  def invalidValue[T](
    in: IN,
    typeName: String,
    path: Path[String]
  ): Left[ExtractionErrors[String], Nothing]

  override def coproductType(in: IN): Option[CoproductType] =
    interchangeFormatPrimitiveValidator.stringValue(in, coproductTypeKey)

  override type CoproductType = String

  protected def isEmpty(json: IN): Boolean

  override def keyDefinition[A](
    value: KeyDefinition[String, ALG, A]
  ): Validator[String, ALG, A, IN] = { (in, path) =>
    headValue(in, value, determineValidator(value.dataDefinition), path)
  }

  protected def determineValidator[A](
    value: Either[HigherOrderValue[String, ALG, A], ALG[A]]
  ): OptionalInputValidator[String, ALG, A, IN] = {
    value match {
      case Left(kvp)  => valueDefinition(kvp)
      case Right(ext) => interchangeFormatValidator.createValidator(ext)
    }
  }

  protected def valueDefinition[A](
    fgo: HigherOrderValue[String, ALG, A]
  ): OptionalInputValidator[String, ALG, A, IN] = {
    val result: OptionalInputValidator[String, ALG, A, IN] =
      fgo match {
        case op: OptionalValue[String, ALG, a] @unchecked =>
          val applied = determineValidator(op.valueDefinitionOp)
          (in: Option[IN], path: Path[String]) =>
            in match {
              case None                  => Right(None)
              case Some(n) if isEmpty(n) => Right(None)
              case some @ Some(json)     => applied.validateWithPath(some, path).map(Some(_))
            }

        case ed: EitherData[String, ALG, a, b] @unchecked =>
          val optionalA = determineValidator(ed.definitionA)
          val optionalB = determineValidator(ed.definitionB)
          (in: Option[IN], path: Path[String]) => {
            optionalA.validateWithPath(in, path) match {
              case Left(err1) =>
                optionalB.validateWithPath(in, path) match {
                  case Right(b) => Right(Right(b))
                  case Left(err2) => {
                    val errors = RequiredValue(path, ed.typeNameOfA) :: err2 ::: err1
                    Left(errors)
                  }
                }
              case Right(a) => Right(Left(a))
            }
          }
        case op: ListData[String, ALG, t] @unchecked =>
          val valueF = determineValidator(op.tDefinition)

          def appendArrayIndex(path: Path[String], index: Int): List[String] = {
            if (path.isEmpty) path
            else
              path.updated(path.length - 1, path.lastOption.getOrElse("") + s"[${index}]")
          }

          def traverseArray(
            arr: Seq[IN],
            path: Path[String]
          ): Either[ExtractionErrors[String], List[t]] = {
            val arrayApplied: Seq[Either[ExtractionErrors[String], t]] =
              arr.zipWithIndex.map(jValue =>
                valueF.validateWithPath(Some(jValue._1), appendArrayIndex(path, jValue._2))
              )

            arrayApplied
              .foldLeft[Either[ExtractionErrors[String], List[t]]](Right(List.empty))((b, v) =>
                (b, v) match {
                  case (Right(a), Right(i)) => Right(a :+ i)
                  case (Left(a), Left(b))   => Left(a ::: b)
                  case (Left(x), _)         => Left(x)
                  case (_, Left(x))         => Left(x)
                }
              )
          }

          (inOpt: Option[IN], path: Path[String]) => {
            val result = for {
              in <- inOpt.toRight(List(RequiredValue(path, op.typeNameOfT)))
              arr <- interchangeFormatPrimitiveValidator
                .extractArray(op.typeName)
                .validateWithPath(in, path)
              listOfIn <- traverseArray(arr, path)
            } yield listOfIn
            result.asInstanceOf[Either[ExtractionErrors[String], A]]
          }
        case op: KvpCollectionValue[String, ALG, A] @unchecked => {
          val fg: Validator[String, ALG, A, IN] =
            fromKvpCollection(op.kvpCollection)
          (jsonOpt: Option[IN], path: Path[String]) => {
            jsonOpt match {
              case Some(json) =>
                fg.validateWithPath(json, path)
                  .flatMap(res => vu.validate[String, A](op.validations)(res, path))
              case None => Left(List(RequiredValue(path, op.typeName)))
            }
          }
        }
      }
    result
  }

}
