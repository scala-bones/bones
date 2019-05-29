package com.bones.interpreter

import java.nio.charset.{Charset, StandardCharsets}
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.{Base64, Date, UUID}

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.Util
import com.bones.data.Error.{
  CanNotConvert,
  ExtractionError,
  RequiredData,
  WrongTypeError
}
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{HList, HNil, Nat,::}
import com.bones.validation.{ValidationUtil => vu}

import scala.util.Try
import scala.util.control.NonFatal

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  * @tparam IN
  */
trait KvpValidateInputInterpreter[IN] {

  import Util._

  type Path = List[String]

  /**
    * Extend this to extract the value of type A from the input type IN
    * @param in The input type, for instance a base Json type.
    * @param kv The key and value definition describing this extraction.
    * @param headInterpreterF we will pass the appropriate extractXXX type based on type A
    * @param path The json path to the element such
    * @tparam A The type being extracted.
    * @return Either successful A or failure.  Should probably just return result from headInterpreterF.
    */
  def headValue[A](in: IN,
                   kv: KeyValueDefinition[A],
                   headInterpreterF: (
                       Option[IN],
                       Path) => Either[NonEmptyList[ExtractionError], A],
                   path: List[String]): Either[NonEmptyList[ExtractionError], A]

  /**
    * Override this to provide the ability to extract a String from the IN type.
    * @param op The string definition.
    * @param clazz The resulting class we are tyring to extract.
    * @param in The interchange format input type.
    * @param path The path hierarchy.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  def extractString[A](op: KvpValue[A], clazz: Class[_])(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], String]
  def extractInt(op: IntData)(
    in: IN,
    path: List[String]): Either[NonEmptyList[ExtractionError], Int]
  def extractLong(op: LongData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], Long]
  def extractBool(op: BooleanData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], Boolean]
  def extractUuid(op: UuidData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], UUID]
  def extractZonedDateTime(dateFormat: DateTimeFormatter, op: DateTimeData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], ZonedDateTime]
  def extractArray[A](op: ListData[A])(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractFloat(op: FloatData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], Float]
  def extractDouble(op: DoubleData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], Double]
  def extractBigDecimal(op: BigDecimalData)(
      in: IN,
      path: List[String]): Either[NonEmptyList[ExtractionError], BigDecimal]
  protected def invalidValue[T](
      in: IN,
      expected: Class[T],
      path: List[String]): Left[NonEmptyList[ExtractionError], Nothing]

  def fromSchema[A](schema: BonesSchema[A])
    : (IN, List[String]) => Either[NonEmptyList[ExtractionError], A] =
    schema match {
      case x: HListConvert[_, _, A] =>
        (in, path) =>
          valueDefinition(x).apply(Some(in), path)
    }

  def byteArrayFuncFromSchema[A](schema: BonesSchema[A], charset: Charset) :
    Array[Byte] => Either[NonEmptyList[ExtractionError],A]

  def required[A](
                   op: KvpValue[A],
                   validations: List[ValidationOp[A]],
                   f: (IN, List[String]) => Either[NonEmptyList[ExtractionError], A],
  ): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
    (inOpt: Option[IN], path: List[String]) =>
      for {
        json <- inOpt
          .toRight(NonEmptyList.one(RequiredData(path, op)))
        a <- f(json, path)
        v <- vu.validate(validations)(a, path)
      } yield a

  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL])
    : (IN, List[String]) => Either[NonEmptyList[ExtractionError], H] = {
    group match {
      case KvpNil =>
        (_: IN, _: List[String]) =>
          Right(HNil)

      case op: KvpHListHead[H, al, h, hl, t, tl] => {
        val headInterpreter = kvpHList(op.head)
        val tailInterpreter = kvpHList(op.tail)

        (in: IN, path: List[String]) =>
          {

            Util
              .eitherMap2(headInterpreter(in, path), tailInterpreter(in, path))(
                (l1: h, l2: t) => {
                  op.prepend.apply(l1, l2)
                })
              .flatMap { l =>
                vu.validate[H](op.validations)(l, path)
              }
          }
      }

      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] => {
        val headInterpreter: (IN, List[String]) => Either[NonEmptyList[ExtractionError], xl] = kvpHList(op.hListConvert.from)
        val tailInterpreter = kvpHList(op.tail)
        (in: IN, path: List[String]) =>
          {
            Util
              .eitherMap2[xl,ht,ho](headInterpreter(in, path), tailInterpreter(in, path))(
                (l1: xl, l2: ht) => {
                  op.isHCons.cons(op.hListConvert.fHtoA(l1), l2)
                })
              .flatMap { l =>
                vu.validate[ho](op.validations)(l, path)
              }
          }
      }

      case op: KvpSingleValueHead[h, t, tl, a] => {

        val headInterpreter = valueDefinition(op.fieldDefinition.op)
        val tailInterpreter = kvpHList(op.tail)

        (in: IN, path: List[String]) =>
          {

            val headPath = path :+ op.fieldDefinition.key

            val head =
              headValue(in, op.fieldDefinition, headInterpreter, headPath)
            val tailValue = tailInterpreter(in, path)

            Util
              .eitherMap2(head, tailValue)((l1, l2) => {
                op.isHCons.cons(l1, l2)
              })
              .flatMap { l =>
                vu.validate(op.validations)(l, path)
              }
          }
      }
    }
  }

  def valueDefinition[A](fgo: KvpValue[A])
    : (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] = {
    val result
      : (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
      fgo match {
        case op: OptionalKvpValueDefinition[a] =>
          val applied = valueDefinition(op.valueDefinitionOp)
          (in: Option[IN], path: List[String]) =>
            in match {
              case None              => Right(None)
              case some @ Some(json) => applied(some, path).map(Some(_))
            }
        case op: StringData =>
          required(op, op.validations, extractString(op, classOf[String]))
        case id: IntData =>
          required(id, id.validations, extractInt(id))
        case op: LongData =>
          required(op, op.validations, extractLong(op))
        case op: BooleanData =>
          required(op, op.validations, extractBool(op))
        case op: UuidData =>
          required(op, op.validations, extractUuid(op))
        case op @ DateTimeData(dateFormat, _, _) =>
          required(op, op.validations, extractZonedDateTime(dateFormat, op))
        case op @ ByteArrayData(validations) =>
          val decoder = Base64.getDecoder
          (inOpt: Option[IN], path: List[String]) =>
            for {
              in <- inOpt.toRight[NonEmptyList[ExtractionError]](
                NonEmptyList.one(RequiredData(path, op)))
              str <- extractString(op, classOf[Array[Byte]])(in, path)
              arr <- Try { decoder.decode(str) }.toEither.left.map(thr =>
                NonEmptyList.one(
                  CanNotConvert(path, str, classOf[Array[Byte]])))
            } yield arr

        case ed: EitherData[a, b] =>
          val optionalA = valueDefinition(ed.definitionA)
          val optionalB = valueDefinition(ed.definitionB)
          (in: Option[IN], path: List[String]) =>
            {
              optionalA(in, path) match {
                case Left(err) =>
                  val nonWrongTypeError = err.toList.filter {
                    case WrongTypeError(_, _, _) => false
                    case RequiredData(_, _)      => false
                    case CanNotConvert(_, _, _)  => false
                    case _                       => true
                  }
                  if (nonWrongTypeError.isEmpty) {
                    optionalB(in, path) match {
                      case Right(b) => Right(Right(b))
                      case Left(err) => {
                        Left(NonEmptyList.one(RequiredData(path, ed)))
                      }
                    }
                  } else {
                    Left(err)
                  }
                case Right(a) => Right(Left(a))
              }
            }
        case op: ListData[t] =>
          val valueF = valueDefinition(op.tDefinition)
          def appendArrayInex(path: List[String], index: Int): List[String] = {
            val size = path.length
            if (path.length == 0) path
            else
              path.updated(path.length - 1,
                           path(path.length - 1) + s"[${index}]")
          }

          def traverseArray(arr: Seq[IN], path: List[String])
            : Either[NonEmptyList[ExtractionError], List[t]] = {
            val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], t]] =
              arr.zipWithIndex.map(jValue =>
                valueF(Some(jValue._1), appendArrayInex(path, jValue._2)))

            arrayApplied
              .foldLeft[Either[NonEmptyList[ExtractionError], List[t]]](
                Right(List.empty))((b, v) =>
                (b, v) match {
                  case (Right(a), Right(i)) => Right(a :+ i)
                  case (Left(a), Left(b))   => Left(a ::: b)
                  case (Left(x), _)         => Left(x)
                  case (_, Left(x))         => Left(x)
              })
          }
          (inOpt: Option[IN], path: List[String]) =>
            {
              for {
                in <- inOpt.toRight(NonEmptyList.one(RequiredData(path, op)))
                arr <- extractArray(op)(in, path)
                listOfIn <- traverseArray(arr, path)
              } yield listOfIn
            }
        case fd: FloatData =>
          required(fd, fd.validations, extractFloat(fd))
        case dd: DoubleData =>
          required(dd, dd.validations, extractDouble(dd))
        case op: BigDecimalData =>
          required(op, op.validations, extractBigDecimal(op))
        case op: EnumerationStringData[A] =>
          (inOpt: Option[IN], path: List[String]) =>
            for {
              in <- inOpt.toRight[NonEmptyList[ExtractionError]](
                NonEmptyList.one(RequiredData(path, op)))
              str <- extractString(op, op.manifestOfA.runtimeClass)(in, path)
              enum <- stringToEnumeration(str,
                                          path,
                                          op.enumeration,
                                          op.manifestOfA)
            } yield enum

        case op: SumTypeData[a, A] =>
          val valueF = valueDefinition(op.from)
          (in: Option[IN], path: List[String]) =>
            {
              valueF(in, path).flatMap(res =>
                op.fab(res, path).left.map(NonEmptyList.one))
            }
        case op: KvpHListValue[h, hl] => {
          val fg: (IN, List[String]) => Either[NonEmptyList[ExtractionError], h] = kvpHList(op.kvpHList)
          (jsonOpt: Option[IN], path: List[String]) =>
            {
              jsonOpt match {
                case Some(json) =>
                  fg(json, path)
                    .flatMap(res => vu.validate[h](op.validations)(res, path))
                    .map(_.asInstanceOf[A])
                case None => Left(NonEmptyList.one(RequiredData(path, op)))
              }
            }
        }
        case x: HListConvert[a, al, A] => {
          val kvp = kvpHList(x.from)
          (jOpt: Option[IN], path: List[String]) =>
            jOpt match {
              case None    => Left(NonEmptyList.one(RequiredData(path, null)))
              case Some(j) => kvp(j, path).right.map(x.fHtoA(_))
            }
        }
      }
    result
  }

}
