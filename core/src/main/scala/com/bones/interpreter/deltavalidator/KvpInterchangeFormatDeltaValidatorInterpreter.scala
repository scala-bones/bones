package com.bones.interpreter.deltavalidator

import com.bones.{Path, Util}
import com.bones.Util.NullableResult
import com.bones.data.Error.ExtractionErrors
import com.bones.data.{
  EitherData,
  HigherOrderValue,
  KeyDefinition,
  KvpCollection,
  KvpCollectionValue,
  KvpCoproduct,
  KvpHListCollectionHead,
  KvpNil,
  KvpSingleValueHead,
  KvpWrappedCoproduct,
  KvpWrappedHList,
  ListData,
  OptionalValue
}
import com.bones.data.template.{
  KvpCollectionFunctor,
  KvpCollectionMatch,
  KvpCollectionValidateAndDecode
}
import com.bones.interpreter.validator.InterchangeFormatPrimitiveValidator
import com.bones.validation.ValidationUtil
import shapeless.HList.ListCompat.::
import shapeless.{::, Coproduct, HList, HNil, Nat}

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  *
  * @tparam IN The base data type of the interchange format, such as io.circe.Json
  */
trait KvpInterchangeFormatDeltaValidatorInterpreter[ALG[_], IN]
    extends KvpCollectionFunctor[String, ALG, DeltaKvpValidator[String, ALG, *, IN]] {

  /** An additional string in the serialized format which states the coproduct type */
  val coproductTypeKey: String

  val interchangeFormatValidator: InterchangeFormatDeltaValidatorValue[ALG, IN]

  val primitiveValidator: InterchangeFormatValidatorNullableValue[IN, String]

  def generateDeltaValidator[A](
    kvpCollection: KvpCollection[String, ALG, A]): DeltaKvpValidator[String, ALG, A, IN] = {
    fromKvpCollection(kvpCollection)
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]): DeltaKvpValidator[String, ALG, A, IN] =
    ???

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C])
    : DeltaKvpValidator[String, ALG, A, IN] =
    ???

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL])
    : DeltaKvpValidator[String, ALG, HO, IN] = ???

  override def kvpNil(kvp: KvpNil[String, ALG]): DeltaKvpValidator[String, ALG, HNil, IN] = ???

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[String, ALG, H, T, TL, O]): DeltaKvpValidator[String, ALG, O, IN] = {

    val headValidator
      : (IN, List[String]) => Either[ExtractionErrors[String], NullableResult[String, H]] =
      kvp.head
        .fold(
          kd => determineValidator(kd.dataDefinition).extractAndValidate(_, kd.key, _),
          kvp => fromKvpCollection(kvp).validate(_, _))

    val tail = fromKvpCollection(kvp.tail)

    (in: IN, path: List[String]) =>
      {
        val headResult = headValidator(in, path)
        val tailResult = tail.validate(in, path)

        Util
          .eitherMap2Nullable(headResult, tailResult)((l1, l2) => {
            kvp.isHCons.cons(l1, l2)
          })
          .flatMap {
            case Left(n)  => Right(Left(n))
            case Right(v) => ValidationUtil.validate(kvp.validations)(v, path).map(Right(_))
          }
      }
  }

  def determineValidator[A](alg: Either[HigherOrderValue[String, ALG, A], ALG[A]])
    : DeltaValueValidator[String, ALG, A, IN] =
    alg match {
      case Left(hov)  => valueDefinition(hov)
      case Right(alg) => interchangeFormatValidator.createDeltaValidator(alg)
    }

  override def kvpCoproduct[C <: Coproduct](
    value: KvpCoproduct[String, ALG, C]): DeltaKvpValidator[String, ALG, C, IN] = ???

  protected def valueDefinition[A](
    fgo: HigherOrderValue[String, ALG, A]): DeltaValueValidator[String, ALG, A, IN] = {
    val result: DeltaValueValidator[String, ALG, A, IN] =
      fgo match {
        case op: OptionalValue[String, ALG, a] @unchecked =>
          determineValidator(op.valueDefinitionOp)
            .map {
              case Left(_)  => Right(None)
              case Right(v) => Right(Some(v))
            }
            .asInstanceOf[DeltaValueValidator[String, ALG, A, IN]]

        case ed: EitherData[String, ALG, a, b] @unchecked =>
          val optionalA = determineValidator(ed.definitionA)
          val optionalB = determineValidator(ed.definitionB)
          optionalA
            .flatMap(x =>
              x match {
                case Left(_)  => optionalB.map(_.map(r => Right(r)))
                case Right(a) => DeltaValueValidator.pureA(a).map(_.map(l => Left(l)))
            })
        case op: ListData[String, ALG, t] @unchecked =>
          val valueF = determineValidator(op.tDefinition)

          def appendArrayIndex(path: Path[String], index: Int): List[String] = {
            if (path.isEmpty) path
            else
              path.updated(path.length - 1, path.lastOption.getOrElse("") + s"[${index}]")
          }

          def traverseArray(
            arr: Seq[IN],
            path: Path[String]): Either[ExtractionErrors[String], Seq[t]] = {

            val arrayApplied: Seq[Either[ExtractionErrors[String], NullableResult[String, t]]] =
              arr.zipWithIndex.map(jValue =>
                valueF.validate(jValue._1, appendArrayIndex(path, jValue._2)))
            arrayApplied
              .foldLeft[Either[ExtractionErrors[String], Seq[t]]](Right(List.empty))(
                (result, next) =>
                  (result, next) match {
                    case (Right(a), Right(nr)) => {
                      val seq: Seq[t] = nr.fold(_ => a, v => v +: a)
                      Right(seq)
                    }
                    case (Left(a), Left(b)) => Left(a ::: b)
                    case (Left(x), _)       => Left(x)
                    case (_, Left(x))       => Left(x)
                })
          }

          val v1 = new DeltaValueValidator[String, ALG, Seq[t], Seq[IN]] {
            override def extract(
              in: Seq[IN],
              key: String): Either[ExtractionErrors[String], NullableResult[String, Seq[IN]]] =
              primitiveValidator.extractArray(op.typeName)(key)

            override def validate(in: Seq[IN], path: List[String])
              : Either[ExtractionErrors[String], NullableResult[String, Seq[t]]] = ???
          }

          val validator = new DeltaValueValidator[String, ALG, Seq[t], IN] {

            override def extract(
              in: IN,
              key: String): Either[ExtractionErrors[String], NullableResult[String, IN]] = ???

            override def validate(in: IN, path: List[String])
              : Either[ExtractionErrors[String], NullableResult[String, Seq[t]]] = ???

            override def extractAndValidate(in: IN, key: String, path: List[String])
              : Either[ExtractionErrors[String], NullableResult[String, Seq[t]]] =
              primitiveValidator.extractArray(op.typeName)(key).flatMap {
                case Left(nr) => Right(Left(nr))
                case Right(seq) => {
                  seq
                    .map(v => valueF.validate(v, path))
                    .foldLeft[Either[ExtractionErrors[String], NullableResult[String, Seq[t]]]](
                      Right(Right(Seq.empty))) { (result, next) =>
                      {
                        (next, result) match {
                          case (Left(err1), Left(err2)) => Left(err1 ::: err2)
                          case (Left(err), _)           => Left(err)
                          case (_, Left(err))           => Left(err)
                          case (Right(h), Right(tail)) =>
                            h match {
                              case Left(_)  => Right(tail)
                              case Right(v) => Right(tail.map(t => v +: t))
                            }
                        }
                      }
                    }
                }
              }
          }
          validator.asInstanceOf[DeltaValueValidator[String, ALG, A, IN]]

        case op: KvpCollectionValue[String, ALG, A] @unchecked => {
          new DeltaValueValidator[String, ALG, A, IN] {
            override def extract(
              in: IN,
              key: String): Either[ExtractionErrors[String], NullableResult[String, IN]] =
              primitiveValidator.extractObject(key)

            override def validate(
              in: IN,
              path: List[String]): Either[ExtractionErrors[String], NullableResult[String, A]] = {
              fromKvpCollection(op.kvpCollection).validate(in, path)
            }

          }

        }
      }
    result
  }

}
