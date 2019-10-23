package com.bones.interpreter

import java.time.{LocalDate, LocalDateTime}
import java.util.{Base64, UUID}

import cats.data.NonEmptyList
//import cats.implicits._
import com.bones.Util
import com.bones.data.Error._
import com.bones.data.{KeyValueDefinition, KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.{ValidationUtil => vu}
import shapeless.{CNil, Coproduct, HList, HNil, Inl, Inr, Nat}

import scala.util.Try

object KvpInterchangeFormatValidatorInterpreter {
  /** Represents a path to an element, such as List("someClass", "someMember", "someField") */
  type Path = List[String]
  type CoproductType = String
  val coproductTypeKey = "type"
}

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  * @tparam IN The base data type of the interchange format, such as io.circe.Json
  */
trait KvpInterchangeFormatValidatorInterpreter[IN] {

  import Util._
  import KvpInterchangeFormatValidatorInterpreter._

  /** Entry point for this class to convert a Schema into a function where
    * the function takes an interchange format, validates it and if successful, spits out
    * the resulting data..
    * @param schema The schema used to validate and decode this input data.
    * @tparam A The final data type returned by the resulting function.
    * @return A function which takes the IN data and returns an Either[ExtractionError,A]
    */
  def fromSchema[A](schema: BonesSchema[A])
  : (IN, List[String]) => Either[NonEmptyList[ExtractionError], A] =
    schema match {
      case x: HListConvert[_, _, A] =>
        (in, path) =>
          valueDefinition(x).apply(Some(in), path)
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
  def headValue[A](in: IN,
                   kv: KeyValueDefinition[A],
                   headInterpreterF: (
                       Option[IN],
                       Path) => Either[NonEmptyList[ExtractionError], A],
                   path: Path): Either[NonEmptyList[ExtractionError], A]

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
      path: Path): Either[NonEmptyList[ExtractionError], String]
  def extractInt(op: IntData)(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Int]
  def extractLong(op: LongData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], Long]
  def extractBool(op: BooleanData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], Boolean]
  def extractUuid(op: UuidData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], UUID]
  def extractLocalDateTime(op: LocalDateTimeData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], LocalDateTime]
  def extractLocalDate(op: LocalDateData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], LocalDate]
  def extractArray[A](op: ListData[A])(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractFloat(op: FloatData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], Float]
  def extractDouble(op: DoubleData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], Double]
  def extractShort(op: ShortData)(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Short]
  def extractBigDecimal(op: BigDecimalData)(
      in: IN,
      path: Path): Either[NonEmptyList[ExtractionError], BigDecimal]
  def stringValue(in: IN, elementName: String): Option[String]
  protected def invalidValue[T](
      in: IN,
      expected: Class[T],
      path: Path): Left[NonEmptyList[ExtractionError], Nothing]

  def required[A](
                   op: KvpValue[A],
                   validations: List[ValidationOp[A]],
                   f: (IN, List[String]) => Either[NonEmptyList[ExtractionError], A],
  ): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
    (inOpt: Option[IN], path: Path) =>
      for {
        json <- inOpt
          .toRight(NonEmptyList.one(RequiredData(path, op)))
        a <- f(json, path)
        v <- vu.validate(validations)(a, path)
      } yield a


  def kvpCoproduct[C<:Coproduct](co: KvpCoproduct[C]):
    (IN, Path, CoproductType) => Either[NonEmptyList[ExtractionError], C] = {
    co match {
      case KvpCoNil => (_:IN, path:List[String], coType: CoproductType) =>
        Left(NonEmptyList.one(SumTypeError(path, s"Unexpected type value: ${coType}")))
      case co: KvpSingleValueLeft[l,r] => {
        val fValue = valueDefinition(co.kvpValue)
        val fTail = kvpCoproduct(co.kvpTail)
        (in, path, coType) => {
          if (coType == co.manifestH.runtimeClass.getSimpleName) fValue(Some(in),path).map(Inl(_))
          else fTail(in,path,coType).map(Inr(_))
        }
      }
    }
  }

  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL])
    : (IN, List[String]) => Either[NonEmptyList[ExtractionError], H] = {
    group match {
      case KvpNil =>
        (_: IN, _: List[String]) =>
          Right(HNil)

      case op: KvpHListHead[H, al, h, hl, t, tl] => {
        val headInterpreter = kvpHList(op.head)
        val tailInterpreter = kvpHList(op.tail)

        (in: IN, path: Path) =>
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
        (in: IN, path: Path) =>
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

        (in: IN, path: Path) =>
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

  def isEmpty(json: IN): Boolean

  def valueDefinition[A](fgo: KvpValue[A])
    : (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] = {
    val result
      : (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
      fgo match {
        case op: OptionalKvpValueDefinition[a] =>
          val applied = valueDefinition(op.valueDefinitionOp)
          (in: Option[IN], path: Path) =>
            in match {
              case None => Right(None)
              case Some(n) if isEmpty(n) => Right(None)
              case some@Some(json) => applied(some, path).map(Some(_))
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
        case op@LocalDateTimeData(validations) =>
          required(op, validations, extractLocalDateTime(op))
        case op@LocalDateData(validations) =>
          required(op, validations, extractLocalDate(op))
        case op@ByteArrayData(validations) =>
          val decoder = Base64.getDecoder
          (inOpt: Option[IN], path: Path) =>
            for {
              in <- inOpt.toRight[NonEmptyList[ExtractionError]](
                NonEmptyList.one(RequiredData(path, op)))
              str <- extractString(op, classOf[Array[Byte]])(in, path)
              arr <- Try {
                decoder.decode(str)
              }.toEither.left.map(thr =>
                NonEmptyList.one(
                  CanNotConvert(path, str, classOf[Array[Byte]])))
            } yield arr

        case ed: EitherData[a, b] =>
          val optionalA = valueDefinition(ed.definitionA)
          val optionalB = valueDefinition(ed.definitionB)
          (in: Option[IN], path: Path) => {
            optionalA(in, path) match {
              case Left(err) =>
                val nonWrongTypeError = err.toList.filter {
                  case WrongTypeError(_, _, _) => false
                  case RequiredData(_, _) => false
                  case CanNotConvert(_, _, _) => false
                  case _ => true
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

          def appendArrayInex(path: Path, index: Int): List[String] = {
            val size = path.length
            if (path.length == 0) path
            else
              path.updated(path.length - 1,
                path(path.length - 1) + s"[${index}]")
          }

          def traverseArray(arr: Seq[IN], path: Path)
          : Either[NonEmptyList[ExtractionError], List[t]] = {
            val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], t]] =
              arr.zipWithIndex.map(jValue =>
                valueF(Some(jValue._1), appendArrayInex(path, jValue._2)))

            arrayApplied
              .foldLeft[Either[NonEmptyList[ExtractionError], List[t]]](
              Right(List.empty))((b, v) =>
              (b, v) match {
                case (Right(a), Right(i)) => Right(a :+ i)
                case (Left(a), Left(b)) => Left(a ::: b)
                case (Left(x), _) => Left(x)
                case (_, Left(x)) => Left(x)
              })
          }

          (inOpt: Option[IN], path: Path) => {
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
        case sd: ShortData =>
          required(sd, sd.validations, extractShort(sd))
        case op: BigDecimalData =>
          required(op, op.validations, extractBigDecimal(op))
        case op: EnumerationData[e, A] =>
          (inOpt: Option[IN], path: Path) =>
            for {
              in <- inOpt.toRight[NonEmptyList[ExtractionError]](
                NonEmptyList.one(RequiredData(path, op)))
              str <- extractString(op, op.manifestOfA.runtimeClass)(in, path)
              enum <- stringToEnumeration[e, A](str,
                path,
                op.enumeration.asInstanceOf[e])(
                op.manifestOfA)
            } yield enum.asInstanceOf[A]

        case op: SumTypeData[a, A] =>
          val valueF = valueDefinition(op.from)
          (in: Option[IN], path: Path) => {
            valueF(in, path).flatMap(res =>
              op.fab(res, path).left.map(NonEmptyList.one))
          }
        case op: KvpHListValue[h, hl] => {
          val fg: (IN, List[String]) => Either[NonEmptyList[ExtractionError], h] = kvpHList(op.kvpHList)
          (jsonOpt: Option[IN], path: Path) => {
            jsonOpt match {
              case Some(json) =>
                fg(json, path)
                  .flatMap(res => vu.validate[h](op.validations)(res, path))
                  .map(_.asInstanceOf[A])
              case None => Left(NonEmptyList.one(RequiredData(path, op)))
            }
          }
        }
        case co: KvpCoproductValue[c] => {
          val fCo = kvpCoproduct(co.kvpCoproduct)
          (jsonOpt: Option[IN], path: Path) => {
            jsonOpt match {
              case Some(json) => {
                stringValue(json, coproductTypeKey) match {
                  case Some(coType) => fCo(json,path, coType).map(_.asInstanceOf[A])
                  case None => Left(NonEmptyList.one(SumTypeError(path, s"Missing parameter ${coproductTypeKey}")))
                }

              }
              case None => Left(NonEmptyList.one(RequiredData(path, co)))
            }
          }
        }
        case x: HListConvert[a, al, A] => {
          val kvp = kvpHList(x.from)
          (jOpt: Option[IN], path: Path) =>
            jOpt match {
              case None => Left(NonEmptyList.one(RequiredData(path, x)))
              case Some(j) => kvp(j, path).map(x.fHtoA(_))
            }
        }
        case co: KvpCoproductConvert[a,c] => {
          val fCo = kvpCoproduct(co.from)
          (jOpt: Option[IN], path: Path) =>
              jOpt match {
                case None => Left(NonEmptyList.one(RequiredData(path, co)))
                case Some(j) => {
                  stringValue(j, coproductTypeKey) match {
                    case None => Left(NonEmptyList.one(RequiredData(coproductTypeKey :: path, co)))
                    case Some(coproductType) => fCo(j,path, coproductType).map(co.cToA(_))
                  }
                }
              }
        }
      }
    result
  }

}
