package com.bones.interpreter.deltavalidator

import com.bones.Util.{CanBeOmitted, OmittedValue}
import com.bones.data.Error.ExtractionErrors
import com.bones.data._
import com.bones.{Path, Util}
import shapeless.UnaryTCConstraint._
import shapeless.{::, Coproduct, HList, HNil, Nat, UnaryTCConstraint}

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  *
  * @tparam IN The base data type of the interchange format, such as io.circe.Json
  */
trait KvpInterchangeFormatDeltaValidatorInterpreter[ALG[_], IN] {

  /** An additional string in the serialized format which states the coproduct type */
//  val coproductTypeKey: String

  val interchangeFormatValidator: InterchangeFormatDeltaValidatorValue[ALG, IN]

  val primitiveValidator: PrimitiveInterchangeFormat[IN, String]

//  def generateDeltaValidator[A](
//    kvpCollection: KvpCollection[String, ALG, A]): DeltaKvpValidator[String, ALG, A, IN] = {
//    fromKvpCollection(kvpCollection)
//  }

//  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
//    wrappedHList: KvpWrappedHList[String, ALG, A, H, HL]): DeltaKvpValidator[String, ALG, A, IN] =
//    fromKvpCollection(wrappedHList.wrappedEncoding)
//      .map(wrappedHList.fHtoA)
//      .addValidation(ValidationUtil.validate(wrappedHList.validations))

//  override def kvpWrappedCoproduct[A, C <: Coproduct](
//    wrappedCoproduct: KvpWrappedCoproduct[String, ALG, A, C])
//    : DeltaKvpValidator[String, ALG, A, IN] = ???

  def fromKvpHListCollection[H <: HList, HL <: Nat](
    kvp: KvpHListCollection[String, ALG, H, HL]): DeltaKvpValidator[String, ALG, H, IN] =
    kvp match {
      case kvpHList: KvpHListCollectionHead[String, ALG, H, HL, h, hl, t, tl] =>
        kvpHListCollectionHead(kvpHList)
      case kNil: KvpNil[String, ALG] =>
        kvpNil(kNil).asInstanceOf[DeltaKvpValidator[String, ALG, H, IN]]
      case kvpSingle: KvpSingleValueHead[String, ALG, x, xs, xsl, H] =>
        kvpSingleValueHead[x, xs, xsl](kvpSingle)
          .asInstanceOf[DeltaKvpValidator[String, ALG, H, IN]]
    }

  def kvpHListCollectionHead[HO <: HList, NO <: Nat, H <: HList, HL <: Nat, T <: HList, TL <: Nat](
    kvp: KvpHListCollectionHead[String, ALG, HO, NO, H, HL, T, TL])
    : DeltaKvpValidator[String, ALG, HO, IN] = ???
//  {
//    val headValidator =
//      fromKvpHListCollection(kvp.head)
//    val tailValidator = fromKvpHListCollection(kvp.tail)
//
//    implicit val headUtcc = headValidator.hListRR
//    implicit val tailUtcc = tailValidator.hListRR
//    implicit val prepend = kvp.prepend
//
//    implicit def hlistsConstUnaryTC[H <: HList, T <: HList](
//      implicit utct: UnaryTCConstraint[T, Const[H]#λ]) =
//      new UnaryTCConstraint[H :: T, Const[H]#λ] {}
//
//    val utcc: UnaryTCConstraint[HO, NullableResult[String, *]] =
//      UnaryTCConstraint[HO, NullableResult[String, *]]
//
//    val utc = new UnaryTCConstraint[H :: T, Const[H]#λ] {}
//
//    new DeltaKvpValidator[String, ALG, HO, IN] {
//
//      //UnaryTCConstraint[HO,NullableResult[String,*]]
//      override def hListRR: UnaryTCConstraint[HO, NullableResult[String, *]] = utcc
//
//      override def validate(
//        in: IN,
//        path: List[String]): Either[ExtractionErrors[String], NullableResult[String, HO]] = {
//        val headValue = headValidator.validate(in, path)
//        val tailValue = tailValidator.validate(in, path)
//        Util
//          .eitherMap2Nullable(headValue, tailValue)((l1, l2) => {
//            kvp.prepend(l1, l2)
//          })
//          .flatMap {
//            case Left(n)  => Right(Left(n))
//            case Right(v) => ValidationUtil.validate(kvp.validations)(v, path).map(Right(_))
//          }
//      }
//    }
//  }

  def kvpNil(kvp: KvpNil[String, ALG]): DeltaKvpValidator[String, ALG, HNil, IN] =
    new DeltaKvpValidator[String, ALG, HNil, IN] {
      override def hListRR: UnaryTCConstraint[HNil, CanBeOmitted[String, *]] =
        UnaryTCConstraint[HNil, CanBeOmitted[String, *]]

      override def validate(in: IN, path: List[String]): Either[ExtractionErrors[String], HNil] =
        Right(HNil)
    }

  def kvpSingleValueHead[HD, T <: HList, TL <: Nat](
    kvp: KvpSingleValueHead[String, ALG, HD, T, TL, _])
    : DeltaKvpValidator[String, ALG, CanBeOmitted[String, HD] :: T, IN] = {

    val tail = fromKvpHListCollection(kvp.tail)
    val tUnaryTCConstraint = tail.hListRR

    kvp.head
      .fold(
        kd => {
          val headV = determineValidator(kd.dataDefinition).extract(_, kd.key, _)
          new DeltaKvpValidator[String, ALG, CanBeOmitted[String, HD] :: T, IN] {

            override def hListRR
              : UnaryTCConstraint[CanBeOmitted[String, HD] :: T, CanBeOmitted[String, *]] =
              UnaryTCConstraint
                .hlistUnaryTC[HD, T, CanBeOmitted[String, *]](tUnaryTCConstraint)

            override def validate(in: IN, path: List[String])
              : Either[ExtractionErrors[String], CanBeOmitted[String, HD] :: T] = {
              val headResult = headV(in, path)
              val tailResult = tail.validate(in, path)

              Util
                .eitherOmittedUnaryPrepend(headResult, tailResult)(tUnaryTCConstraint)
// TODO: we can't validate because some values may be null.
// If there is a mix of null/non null values, we should probably fail.
//              combined.flatMap {
//                  case Left(n)  => Right(Left(n))
//                  case Right(v) => ValidationUtil.validate(kvp.validations)(v, path).map(Right(_))
//                }
            }
          }
        },
        kvp => ???
      )

  }

  def determineValidator[A](alg: Either[HigherOrderValue[String, ALG, A], ALG[A]])
    : DeltaValueValidator[String, ALG, A, IN] =
    alg match {
      case Left(hov)  => valueDefinition(hov)
      case Right(alg) => interchangeFormatValidator.createDeltaValidator(alg)
    }

  def kvpCoproduct[C <: Coproduct](value: KvpCoproduct[String, ALG, C]) = ???

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

          val validator = new DeltaValueValidator[String, ALG, Seq[t], IN] {

            override def extract(
              in: IN,
              key: String,
              path: List[String]): Either[ExtractionErrors[String], CanBeOmitted[String, Seq[t]]] =
              primitiveValidator.extractArray(op.typeName).extract(in, key, path).flatMap {
                case Left(nr) => Right(Left(nr))
                case Right(seq) => {
                  seq
                    .map(v => valueF.extract(v, key, path))
                    .foldLeft[Either[ExtractionErrors[String], CanBeOmitted[String, Seq[t]]]](
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

        case op: KvpCollectionValue[String, ALG, A] @unchecked =>
          DeltaValueValidator.pure(Left(List(OmittedValue("unknown", op.typeName, List.empty)))) //TODO: This is not implemented correctly
      }
    result
  }

}
