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
import shapeless.{HList, HNil, Nat}
import com.bones.validation.{ValidationUtil => vu}

import scala.util.control.NonFatal


trait KvpValidateInputInterpreter[IN] {

  def headValue[A](in: IN,
                   kv: KeyValueDefinition[A],
                   headInterpreter: Option[IN] =>  Either[NonEmptyList[ExtractionError],A]
                  ): Either[NonEmptyList[ExtractionError],A]

  def extractString(in: IN): Either[NonEmptyList[WrongTypeError[String]], String]
  def extractLong(in: IN): Either[NonEmptyList[WrongTypeError[Long]], Long]
  def extractBool(in: IN): Either[NonEmptyList[WrongTypeError[Boolean]], Boolean]
  def extractUuid(in: IN): Either[NonEmptyList[ExtractionError], UUID]
  def extractZonedDateTime(in: IN, dateFormat: DateTimeFormatter): Either[NonEmptyList[ExtractionError], ZonedDateTime]
  def extractArray(in: IN): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractBigDecimal(in: IN): Either[NonEmptyList[ExtractionError], BigDecimal]
  protected def invalidValue[T](in: IN, expected: Class[T]): Left[NonEmptyList[WrongTypeError[T]], Nothing]

  def requiredConvert[A, B](
                             op: ValueDefinitionOp[B],
                             inOpt: Option[IN],
                             f: IN => Option[A],
                             convert: A => Either[NonEmptyList[ExtractionError], B]) =
    for {
      in <- inOpt
        .toRight(NonEmptyList.one(RequiredData(op)))
        .asInstanceOf[Either[NonEmptyList[ExtractionError], IN]]
      a <- f(in)
        .toRight(NonEmptyList.one(invalidValue(in, classOf[Object])))
    } yield a

  def required[A](
                   op: ValueDefinitionOp[A],
                   inOpt: Option[IN],
                   f: IN => Either[NonEmptyList[ExtractionError],A]): Either[NonEmptyList[ExtractionError], A] =
    for {
      json <- inOpt
        .toRight(NonEmptyList.one(RequiredData(op)))
        .asInstanceOf[Either[NonEmptyList[ExtractionError], IN]]
      a <- f(json)
    } yield a


  def dataClass[A](value: DataClass[A]): Option[IN] => Either[NonEmptyList[ExtractionError],A] = {
    value match {
      case x: XMapData[a, al, b] => {
        val kvp = kvpGroup(x.from)
        (jOpt: Option[IN]) =>
          jOpt match {
            case None    => Left(NonEmptyList.one(RequiredData(null)))
            case Some(j) => kvp.apply(j).right.map(x.fab(_).asInstanceOf[A])
          }
      }
      case op: OptionalDataClass[a] => {
        val dcF = dataClass(op.value)
        (jOpt: Option[IN]) =>
          jOpt match {
            case None    => Right(None).map(_.asInstanceOf[A])
            case Some(j) => dcF.apply(Some(j)).map(_.asInstanceOf[A])
          }
      }
    }
  }


  def kvpGroup[H <: HList, HL <: Nat](group: KvpGroup[H, HL]): IN => Either[NonEmptyList[ExtractionError],H] = {
    group match {
      case KvpNil =>
        (_: IN) =>
          Right(HNil)

      case op: OptionalKvpGroup[h,hl] => ???

      case op: KvpGroupHead[H, al, h, hl, t, tl] => {
        val headInterpreter = kvpGroup(op.head)
        val tailInterpreter = kvpGroup(op.tail)

        (in: IN) => {

          Applicative[({
            type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]
          })#AL]
            .map2(headInterpreter(in).toValidated,
              tailInterpreter(in).toValidated)((l1: h, l2: t) => {
              op.prepend.apply(l1, l2)
            })
            .andThen { l =>
              vu.validate[H](l, op.validations)
                .asInstanceOf[Validated[NonEmptyList[ExtractionError], H]]
            }
            .toEither
        }
      }

      case op: KvpSingleValueHead[h, t, tl, a] => {


        val headInterpreter = valueDefinition(op.fieldDefinition.op)
        val tailInterpreter = kvpGroup(op.tail)

        (in: IN) => {

          val head = headValue(in, op.fieldDefinition, headInterpreter)
          val tailValue = tailInterpreter(in)

          Applicative[({
            type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]
          })#AL]
            .map2(head.toValidated, tailValue.toValidated)((l1, l2) => {
              l1.asInstanceOf[h] :: l2.asInstanceOf[t]
            })
            .toEither
            .flatMap { l =>
              vu.validate(l.asInstanceOf[a], op.validations)
            }
            .asInstanceOf[Either[NonEmptyList[ExtractionError], H]]
        }
      }
      case dc: KvpDataClassHead[h, t, tl, o] => {
        val dcF = dataClass(dc.dataClass)
        val tailF = kvpGroup(dc.tail)
        in: IN =>
        {
          Applicative[({
            type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]
          })#AL]
            .map2(dcF(Some(in)).toValidated, tailF(in).toValidated)(
              (l1, l2) => {
                l1.asInstanceOf[h] :: l2.asInstanceOf[t]
              })
            .toEither
            .flatMap { l =>
              vu.validate(l.asInstanceOf[o], dc.validations)
            }
            .asInstanceOf[Either[NonEmptyList[ExtractionError], H]]
        }
      }

    }
  }


  def stringToUuid(uuidString: String): Either[NonEmptyList[ExtractionError], UUID] = try {
    Right(UUID.fromString(uuidString))
  } catch {
    case _: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
  }

  def stringToZonedDateTime(input: String, dateFormat: DateTimeFormatter): Either[NonEmptyList[ExtractionError], ZonedDateTime] = try {
    Right(ZonedDateTime.parse(input, dateFormat))
  } catch {
    case NonFatal(ex) => Left(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
  }



  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Option[IN] => Either[NonEmptyList[ExtractionError],A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] =>
        val applied = valueDefinition(op.valueDefinitionOp)
        (in: Option[IN]) =>
          in match {
            case None              => Right(None)
            case some @ Some(json) => applied(some).map(Some(_))
          }
      case op: StringData =>
        required(op, _: Option[IN], extractString)
          .flatMap(vu.validate(_, op.validations))
      case op: LongData =>
        required(op, _: Option[IN], extractLong)
          .flatMap(vu.validate(_, op.validations))
      case op: BooleanData =>
        required(op, _: Option[IN], extractBool)
          .flatMap(vu.validate(_, op.validations))
      case op: UuidData => {
        required(op, _: Option[IN], extractUuid)
          .flatMap(vu.validate(_, op.validations))
      }
      case op @ DateTimeData(dateFormat, _, _) =>
        required(op, _:Option[IN], extractZonedDateTime(_,dateFormat))
          .flatMap(vu.validate(_, op.validations))
      case ed: EitherData[a, b] =>
        val optionalA = valueDefinition(ed.definitionA)
        val optionalB = valueDefinition(ed.definitionB)
        in: Option[IN] =>
        {
          optionalA(in) match {
            case Left(err) =>
              val nonWrongTypeError = err.toList.filter {
                case WrongTypeError(_,_) => false
                case RequiredData(_) => false
                case _ => true
              }
              if (nonWrongTypeError.isEmpty) {
                optionalB(in) match {
                  case Right(b) => Right(Right(b))
                  case Left(err) => {
                    Left(NonEmptyList.one(RequiredData(ed)))
                  }
                }
              } else {
                Left(err)
              }
            case Right(a) => Right(Left(a))
          }
        }
      case op: ListData[t, l] =>
        val valueF = valueDefinition(op)
        def traverseArray(arr: Seq[IN]) = {
          arr
            .map(jValue => valueF.apply(Some(jValue)))
            .foldLeft[Either[NonEmptyList[ExtractionError], List[_]]](
            Right(List.empty))((b, v) =>
            (b, v) match {
              case (Right(a), Right(i)) => Right(a :+ i)
              case (Left(a), Left(b))   => Left(a ::: b)
              case (Left(x), _)         => Left(x)
              case (_, Left(x))         => Left(x)
            })
        }
        inOpt: Option[IN] =>
        {
          for {
            in <- inOpt.toRight(NonEmptyList.one(RequiredData(op)))
            arr <- extractArray(in)
            result <- traverseArray(arr)
          } yield result
        }
      case op: BigDecimalData =>
        jsonOpt: Option[IN] =>
          required(op, jsonOpt, extractBigDecimal)
            .flatMap(vu.validate(_, op.validations))
      case op: EnumerationStringData[a] =>

        def strToEnumeration(str: String): Either[NonEmptyList[CanNotConvert[String,Object]],A] = try {
          Right(op.enumeration.withName(str).asInstanceOf[A])
        } catch {
          case ex: NoSuchElementException =>
            Left(
              NonEmptyList.one(CanNotConvert(str, classOf[Object])))
        }

        (inOpt: Option[IN]) => for {
          in <- inOpt.toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(op)))
          str <- extractString(in)
          enum <- strToEnumeration(str)
        } yield enum

      case op: EnumStringData[A] =>
        def strToEnum(str: String): Either[NonEmptyList[CanNotConvert[String,Object]],A] =
          op.enums
            .find(_.toString === str)
            .toRight(NonEmptyList.one(CanNotConvert(str, classOf[Object])))

        (inOpt: Option[IN]) => for {
          in <- inOpt.toRight[NonEmptyList[ExtractionError]](
            NonEmptyList.one(RequiredData(op)))
          str <- extractString(in)
          enum <- strToEnum(str)
        } yield enum

      case op: SumTypeData[a, A] =>
        val valueF = valueDefinition(op.from)
        (in: Option[IN]) =>
        {
          valueF.apply(in).flatMap(res => op.fab.apply(res))
        }
      case op: KvpGroupData[h, hl] => {
        val fg = kvpGroup(op.kvpGroup)
        (jsonOpt: Option[IN]) =>
        {
          jsonOpt match {
            case Some(json) =>
              fg(json).flatMap(res => vu.validate(res, op.validations))
            case None => Left(NonEmptyList.one(RequiredData(op)))
          }
        }
      }

    }
    result.asInstanceOf[Option[IN] => Either[NonEmptyList[ExtractionError],A]]
  }


}
