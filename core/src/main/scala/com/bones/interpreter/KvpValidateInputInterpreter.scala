package com.bones.interpreter

import java.nio.charset.{Charset, StandardCharsets}
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{HList, HNil, Nat}
import com.bones.validation.{ValidationUtil => vu}

import scala.util.control.NonFatal


object KvpValidateInputInterpreter {
  def stringToUuid(uuidString: String, path: Vector[String]): Either[NonEmptyList[ExtractionError], UUID] = try {
    Right(UUID.fromString(uuidString))
  } catch {
    case _: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(path, uuidString, classOf[UUID])))
  }

  def stringToZonedDateTime(input: String, dateFormat: DateTimeFormatter, path: Vector[String]): Either[NonEmptyList[ExtractionError], ZonedDateTime] = try {
    Right(ZonedDateTime.parse(input, dateFormat))
  } catch {
    case NonFatal(ex) => Left(NonEmptyList.one(CanNotConvert(path, input, classOf[Date])))
  }

  def stringToBigDecimal(input: String, path: Vector[String]): Either[NonEmptyList[ExtractionError], BigDecimal] = try {
    Right(BigDecimal(input))
  } catch {
    case _: NumberFormatException => Left(NonEmptyList.one(CanNotConvert(path, input, classOf[BigDecimal])))
  }

  def stringToEnumeration[A](str: String, path: Vector[String], enumeration: Enumeration, manifest: Manifest[A]): Either[NonEmptyList[CanNotConvert[String,Object]],A] = try {
    val clazz = manifest.runtimeClass.asInstanceOf[Class[A]]
    Right(clazz.cast(enumeration.withName(str)))
  } catch {
    case ex: NoSuchElementException =>
      Left(
        NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))
  }

  def stringToEnum[A <: Enum[A]](str: String, path: Vector[String], enums: List[A]): Either[NonEmptyList[CanNotConvert[String,Object]],A] =
    enums
      .find(_.toString === str)
      .toRight(NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))

}

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  * @tparam IN
  */
trait KvpValidateInputInterpreter[IN] {

  import KvpValidateInputInterpreter._

  def headValue[A](in: IN,
                   kv: KeyValueDefinition[A],
                   headInterpreter: (Option[IN], Vector[String]) =>  Either[NonEmptyList[ExtractionError],A],
                   path: Vector[String]
                  ): Either[NonEmptyList[ExtractionError],A]

  def extractString[A](op: ValueDefinitionOp[A], clazz: Class[_])(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], String]
  def extractLong(op: LongData)(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], Long]
  def extractBool(op: BooleanData)(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], Boolean]
  def extractUuid(op: UuidData)(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], UUID]
  def extractZonedDateTime(dateFormat: DateTimeFormatter, op: DateTimeData)(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], ZonedDateTime]
  def extractArray[A](op: ListData[A])(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractBigDecimal(op: BigDecimalData)(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], BigDecimal]
  protected def invalidValue[T](in: IN, expected: Class[T], path: Vector[String]): Left[NonEmptyList[ExtractionError], Nothing]

  def fromSchema[A](schema: BonesSchema[A]) : (IN, Vector[String]) => Either[NonEmptyList[ExtractionError],A] = schema match {
    case x: XMapData[_,_,A] => (in, path)  => valueDefinition(x).apply(Some(in), path)
  }

  def required[A](
                   op: ValueDefinitionOp[A],
                   validations: List[ValidationOp[A]],
                   f: (IN,Vector[String]) => Either[NonEmptyList[ExtractionError],A],
                 ): (Option[IN], Vector[String]) => Either[NonEmptyList[ExtractionError], A] =
    (inOpt: Option[IN], path: Vector[String]) =>
    for {
      json <- inOpt
        .toRight(NonEmptyList.one(RequiredData(path, op)))
      a <- f(json, path)
      v <- vu.validate(validations)(a,path)
    } yield a


  def kvpGroup[H <: HList, HL <: Nat](group: KvpGroup[H, HL]): (IN, Vector[String]) => Either[NonEmptyList[ExtractionError],H] = {
    group match {
      case KvpNil =>
        (_: IN, _: Vector[String]) =>
          Right(HNil)

      case op: OptionalKvpGroup[h,hl] => ???

      case op: KvpGroupHead[H, al, h, hl, t, tl] => {
        val headInterpreter = kvpGroup(op.head)
        val tailInterpreter = kvpGroup(op.tail)

        (in: IN, path: Vector[String]) => {

          Applicative[({
            type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]
          })#AL]
            .map2(headInterpreter(in, path).toValidated,
              tailInterpreter(in, path).toValidated)((l1: h, l2: t) => {
              op.prepend.apply(l1, l2)
            }).toEither
            .flatMap { l =>
              vu.validate[H](op.validations)(l,path)
            }

        }
      }

      case op: KvpSingleValueHead[h, t, tl, a] => {


        val headInterpreter = valueDefinition(op.fieldDefinition.op)
        val tailInterpreter = kvpGroup(op.tail)

        (in: IN, path: Vector[String]) => {

          val headPath = path :+ op.fieldDefinition.key

          val head = headValue(in, op.fieldDefinition, headInterpreter, headPath)
          val tailValue = tailInterpreter(in, path)

          Applicative[({
            type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]
          })#AL]
            .map2(head.toValidated, tailValue.toValidated)((l1, l2) => {
              l1.asInstanceOf[h] :: l2.asInstanceOf[t]
            })
            .toEither
            .flatMap { l =>
              vu.validate(op.validations)(l.asInstanceOf[a], path)
            }
            .asInstanceOf[Either[NonEmptyList[ExtractionError], H]]
        }
      }
    }
  }


  def valueDefinition[A](fgo: ValueDefinitionOp[A]): (Option[IN], Vector[String]) => Either[NonEmptyList[ExtractionError],A] = {
    val result: (Option[IN], Vector[String]) => Either[NonEmptyList[ExtractionError],A] = fgo match {
      case op: OptionalValueDefinition[a] =>
        val applied = valueDefinition(op.valueDefinitionOp)
        (in: Option[IN], path: Vector[String]) =>
          in match {
            case None              => Right(None)
            case some @ Some(json) => applied(some, path).map(Some(_))
          }
      case op: StringData =>
        required(op, op.validations, extractString(op, classOf[String]))
      case op: LongData =>
        required(op, op.validations, extractLong(op))
      case op: BooleanData =>
        required(op, op.validations, extractBool(op))
      case op: UuidData =>
        required(op, op.validations, extractUuid(op))
      case op @ DateTimeData(dateFormat, _, _) =>
        required(op, op.validations, extractZonedDateTime(dateFormat,op))
      case ed: EitherData[a, b] =>
        val optionalA = valueDefinition(ed.definitionA)
        val optionalB = valueDefinition(ed.definitionB)
        (in: Option[IN], path: Vector[String]) =>
        {
          optionalA(in,path) match {
            case Left(err) =>
              val nonWrongTypeError = err.toList.filter {
                case WrongTypeError(_,_,_) => false
                case RequiredData(_,_) => false
                case CanNotConvert(_,_,_) => false
                case _ => true
              }
              if (nonWrongTypeError.isEmpty) {
                optionalB(in,path) match {
                  case Right(b) => Right(Right(b))
                  case Left(err) => {
                    Left(NonEmptyList.one(RequiredData(path,ed)))
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
        def appendArrayInex(path: Vector[String], index: Int) : Vector[String] = {
          val size = path.length
          if (path.length == 0) path
          else path.updated(path.length - 1, path(path.length - 1) + s"[${index}]")
        }

        def traverseArray(arr: Seq[IN], path: Vector[String]): Either[NonEmptyList[ExtractionError], List[t]] = {
          val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], t]] =
            arr.zipWithIndex.map(jValue => valueF(Some(jValue._1), appendArrayInex(path, jValue._2)))

          arrayApplied.foldLeft[Either[NonEmptyList[ExtractionError], List[t]]](
              Right(List.empty))((b, v) =>
            (b, v) match {
              case (Right(a), Right(i)) => Right(a :+ i)
              case (Left(a), Left(b))   => Left(a ::: b)
              case (Left(x), _)         => Left(x)
              case (_, Left(x))         => Left(x)
            })
        }
        (inOpt: Option[IN], path: Vector[String]) => {
          for {
            in <- inOpt.toRight(NonEmptyList.one(RequiredData(path, op)))
            arr <- extractArray(op)(in, path)
            listOfIn <- traverseArray(arr, path)
          } yield listOfIn
        }
      case op: BigDecimalData =>
          required(op, op.validations, extractBigDecimal(op))
      case op: EnumerationStringData[A] =>
        (inOpt: Option[IN], path: Vector[String]) => for {
          in <- inOpt.toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(path, op)))
          str <- extractString(op, op.manifestOfA.runtimeClass)(in,path)
          enum <- stringToEnumeration(str, path, op.enumeration, op.manifestOfA)
        } yield enum

      case op: EnumStringData[a] =>
        (inOpt: Option[IN], path: Vector[String]) => for {
          in <- inOpt.toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(path, op)))
          str <- extractString(op, op.manifestOfA.runtimeClass)(in,path)
          enum <- stringToEnum[a](str,path,op.enums)
        } yield enum.asInstanceOf[A]

      case op: SumTypeData[a, A] =>
        val valueF = valueDefinition(op.from)
        (in: Option[IN], path: Vector[String]) =>
        {
          valueF(in, path).flatMap(res => op.fab(res, path).left.map(NonEmptyList.one))
        }
      case op: KvpGroupData[h, hl] => {
        val fg = kvpGroup(op.kvpGroup)
        (jsonOpt: Option[IN], path: Vector[String]) =>
        {
          jsonOpt match {
            case Some(json) =>
              fg(json, path)
                .flatMap(res => vu.validate(op.validations)(res, path))
                .map(_.asInstanceOf[A])
            case None => Left(NonEmptyList.one(RequiredData(path, op)))
          }
        }
      }
      case x: XMapData[a, al, A] => {
        val kvp = kvpGroup(x.from)
        (jOpt: Option[IN], path: Vector[String]) =>
          jOpt match {
            case None    => Left(NonEmptyList.one(RequiredData(path, null)))
            case Some(j) => kvp(j,path).right.map(x.fab(_))
          }
      }
    }
    result
  }


}
