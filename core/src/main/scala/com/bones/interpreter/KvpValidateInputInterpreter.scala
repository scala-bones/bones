package com.bones.interpreter

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


trait KvpValidateInputInterpreter[IN] {

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
  def extractXMapArray[A](op: XMapListData[A])(in: IN, path: Vector[String]): Either[NonEmptyList[ExtractionError], Seq[IN]]
  protected def invalidValue[T](in: IN, expected: Class[T], path: Vector[String]): Left[NonEmptyList[ExtractionError], Nothing]

//  def requiredConvert[A, B](
//                             op: ValueDefinitionOp[B],
//                             inOpt: Option[IN],
//                             f: IN => Option[A],
//                             convert: A => Either[NonEmptyList[ExtractionError], B]): Either[NonEmptyList[ExtractionError], A] =
//    for {
//      in <- inOpt
//        .toRight(NonEmptyList.one(RequiredData(op)))
//      a <- f(in).map(Right(_)).getOrElse(invalidValue(in, classOf[Object]))
//    } yield a

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


  def dataClass[A](value: DataClass[A]): (Option[IN], Vector[String]) => Either[NonEmptyList[ExtractionError],A] = {
    value match {
      case x: XMapData[a, al, b] => {
        val kvp = kvpGroup(x.from)
        (jOpt: Option[IN], path: Vector[String]) =>
          jOpt match {
            case None    => Left(NonEmptyList.one(RequiredData(path, null)))
            case Some(j) => kvp(j,path).right.map(x.fab(_))
          }
      }
      case dc: OptionalDataClass[a] => {
        val dcF = dataClass(dc.value)
        (jOpt: Option[IN], path: Vector[String]) =>
          jOpt match {
            case None    => Right(None)
            case Some(j) => dcF(Some(j), path).asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
          }
      }
      case op: XMapListData[b] => {
        val valueF = dataClass(op.value)
        def traverseArray(arr: Seq[IN], path: Vector[String]): Either[NonEmptyList[ExtractionError], List[b]] = {
          val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], b]] = arr.map(jValue => valueF(Some(jValue), path))
          arrayApplied.foldLeft[Either[NonEmptyList[ExtractionError], List[b]]](
            Right(List.empty))((b, v) =>
            (b, v) match {
              case (Right(a), Right(i)) => Right(a :+ i)
              case (Left(a), Left(b))   => Left(a ::: b)
              case (Left(x), _)         => Left(x)
              case (_, Left(x))         => Left(x)
            })
        }
        (inOpt: Option[IN], path: Vector[String]) =>
        {
          for {
            in <- inOpt.toRight(NonEmptyList.one(RequiredData(path,null)))
            arr <- extractXMapArray(op)(in, path)
            listOfIn <- traverseArray(arr, path)
          } yield listOfIn
        }
      }
    }
  }


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

          val head = headValue(in, op.fieldDefinition, headInterpreter, path)
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
      case dc: KvpDataClassHead[h, t, tl, o] => {
        val dcF = dataClass(dc.dataClass)
        val tailF = kvpGroup(dc.tail)
        (in: IN, path: Vector[String]) =>
        {
          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(dcF(Some(in), path).toValidated, tailF(in, path).toValidated)(
              (l1, l2) => {
                l1.asInstanceOf[h] :: l2.asInstanceOf[t]
              })
            .toEither
            .flatMap { l =>
              vu.validate(dc.validations)(l.asInstanceOf[o], path)
            }
            .asInstanceOf[Either[NonEmptyList[ExtractionError], H]]
        }
      }

    }
  }


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
        def traverseArray(arr: Seq[IN], path: Vector[String]): Either[NonEmptyList[ExtractionError], List[t]] = {
          val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], t]] = arr.map(jValue => valueF(Some(jValue), path))
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
      case op: EnumerationStringData[a] =>

        def strToEnumeration(str: String, path: Vector[String]): Either[NonEmptyList[CanNotConvert[String,Object]],A] = try {
          Right(op.enumeration.withName(str).asInstanceOf[A])
        } catch {
          case ex: NoSuchElementException =>
            Left(
              NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))
        }

        (inOpt: Option[IN], path: Vector[String]) => for {
          in <- inOpt.toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(path, op)))
          str <- extractString(op, op.manifestOfA.runtimeClass)(in,path)
          enum <- strToEnumeration(str, path)
        } yield enum

      case op: EnumStringData[A] =>
        def strToEnum(str: String, path: Vector[String]): Either[NonEmptyList[CanNotConvert[String,Object]],A] =
          op.enums
            .find(_.toString === str)
            .toRight(NonEmptyList.one(CanNotConvert(path, str, classOf[Object])))

        (inOpt: Option[IN], path: Vector[String]) => for {
          in <- inOpt.toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(path, op)))
          str <- extractString(op, op.manifestOfA.runtimeClass)(in,path)
          enum <- strToEnum(str,path)
        } yield enum

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
      case op: KvpValueData[a] => {
        dataClass(op.value)
      }

    }
    result
  }


}
