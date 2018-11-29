package com.bones.bson

import java.time.{Instant, ZoneOffset, ZonedDateTime}
import java.util.UUID

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value.{ValueDefinitionOp, _}
import com.bones.validation.{ValidationUtil => vu}
import reactivemongo.bson.{BSONArray, BSONBoolean, BSONDateTime, BSONDocument, BSONDouble, BSONLong, BSONString, BSONValue}
import shapeless.{HList, HNil, Nat}

trait KvpInterpreter[IN] {


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

  def headValue[A](in: IN,
                   fieldKey: String,
                   headInterpreter: Option[IN] =>  Either[NonEmptyList[ExtractionError],A]
                  ): Either[NonEmptyList[ExtractionError],A]

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

          val head = headValue(in, op.fieldDefinition.key, headInterpreter)
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


  def extractString(in: IN): Either[NonEmptyList[WrongTypeError[String]], String]
  def extractLong(in: IN): Either[NonEmptyList[WrongTypeError[Long]], Long]
  def extractBool(in: IN): Either[NonEmptyList[WrongTypeError[Boolean]], Boolean]
  def extractUuid(in: IN): Either[NonEmptyList[ExtractionError], UUID]
  def extractZonedDateTime(in: IN): Either[NonEmptyList[WrongTypeError[ZonedDateTime]], ZonedDateTime]
  def extractArray(in: IN): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractBigDecimal(in: IN): Either[NonEmptyList[ExtractionError], BigDecimal]

  def stringToUuid(uuidString: String): Either[NonEmptyList[ExtractionError], UUID] = try {
    Right(UUID.fromString(uuidString))
  } catch {
    case _: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
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
        required(op, _:Option[IN], extractZonedDateTime)
          .flatMap(vu.validate(_, op.validations))
      case ed: EitherData[a, b] =>
        val optionalA = valueDefinition(OptionalValueDefinition(ed.definitionA))
        val optionalB = valueDefinition(OptionalValueDefinition(ed.definitionB))
        in: Option[IN] =>
        {
          optionalA(in).right.flatMap {
            case Some(a) => Right(Left(a))
            case None => {
              optionalB(in).right.flatMap {
                case Some(b) => Right(Right(b))
                case None    => Left(NonEmptyList.one(RequiredData(ed)))
              }
            }
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

class ValidatedFromBsonInterpreter extends KvpInterpreter[BSONValue] {

  type ValidatedFromJsonOption[A] =
    Option[BSONValue] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJson[A] =
    BSONValue => Either[NonEmptyList[ExtractionError], A]

  def invalidValue[T](bson: BSONValue,
                                expected: Class[T]): Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = bson match {
      case _: BSONBoolean  => classOf[Boolean]
      case _: BSONDouble   => classOf[Double]
      case _: BSONString   => classOf[String]
      case _: BSONArray    => classOf[Array[_]]
      case _: BSONDocument => classOf[Object]
      case _               => classOf[Any]
    }
    Left(NonEmptyList.one(WrongTypeError(expected, invalid)))
  }

  def headValue[A](in: BSONValue,
                    fieldKey: String,
                    headInterpreter: Option[BSONValue] =>  Either[NonEmptyList[ExtractionError],A]
                   ): Either[NonEmptyList[ExtractionError],A] = {
    in match {
      case doc: BSONDocument =>
        val fields = doc.elements
        headInterpreter(
          fields.find(_.name == fieldKey).map(_.value))
      case _ => invalidValue(in, classOf[BSONDocument])
    }

  }

  override def extractString(in: BSONValue): Either[NonEmptyList[WrongTypeError[String]], String] =
    in match {
      case BSONString(str) => Right(str)
      case x => invalidValue(x, classOf[String])
    }

  override def extractLong(in: BSONValue): Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in match {
      case BSONLong(long) => Right(long)
      case x => invalidValue(x, classOf[Long])
    }

  override def extractBool(in: BSONValue): Either[NonEmptyList[WrongTypeError[Boolean]], Boolean] =
    in match{
      case BSONBoolean(bool) => Right(bool)
      case x => invalidValue(x, classOf[Boolean])
    }

  override def extractUuid(in: BSONValue): Either[NonEmptyList[ExtractionError], UUID] =
    in match {
      case BSONString(str) => stringToUuid(str)
      case x => invalidValue(x, classOf[UUID])
    }

  override def extractZonedDateTime(in: BSONValue): Either[NonEmptyList[WrongTypeError[ZonedDateTime]], ZonedDateTime] =
    in match {
      case BSONDateTime(date) =>
        val i = Instant.ofEpochSecond(date)
        Right(ZonedDateTime.ofInstant(i, ZoneOffset.UTC))
      case x => invalidValue(x, classOf[ZonedDateTime])
    }

  override def extractArray(in: BSONValue): Either[NonEmptyList[ExtractionError], Seq[BSONValue]] =
    in match {
      case BSONArray(arr) =>
        (arr.toList.map(_.toEither.leftMap(NonEmptyList.one).toValidated).sequence).toEither match {
          case Right(s) => Right(s)
          case Left(err) => Left(NonEmptyList.one(CanNotConvert(arr, classOf[Seq[_]])))
        }
      case x => invalidValue(x, classOf[Array[_]])

    }

  override def extractBigDecimal(in: BSONValue): Either[NonEmptyList[ExtractionError], BigDecimal] =
    in match {
      case BSONDouble(d) => Right(BigDecimal(d))
      case x => invalidValue(x, classOf[BigDecimal])
    }
}
