package com.bones.circe

import java.time.ZonedDateTime
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value.{ValueDefinitionOp, _}
import com.bones.validation.{ValidationUtil => vu}
import io.circe.{Json, JsonObject}
import shapeless.{HList, HNil, Nat}

import scala.util.control.NonFatal

object ValidatedFromCirceInterpreter {
//  trait Validated[I,O]
//  case class ValidatedString() extends Validated[String,JString]
//  case class ValidatedObject[A]() extends Validated[A, ]
//
  type ValidatedFromJsonOption[A] = Option[Json] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJson[A] = Json => Either[NonEmptyList[ExtractionError], A]

  protected def invalidValue[T](json: Json, expected: Class[T]): WrongTypeError[T] = {
    val invalid = json.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    WrongTypeError(expected, invalid)
  }

  def required[A](op: ValueDefinitionOp[A], jsonOpt: Option[Json], f: Json => Option[A]): Either[NonEmptyList[ExtractionError],A] =
    for {
      json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op))).asInstanceOf[Either[NonEmptyList[ExtractionError],Json]]
      a <- f(json).toRight(NonEmptyList.one(invalidValue(json, classOf[Object]))).asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
    } yield a

  def requiredConvert[A,B](op: ValueDefinitionOp[B],
                           jsonOpt: Option[Json],
                           f: Json => Option[A],
                           convert: A => Either[NonEmptyList[ExtractionError],B]
                          ): Either[NonEmptyList[ExtractionError],B] =
    for {
      json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
      a <- f(json).toRight(NonEmptyList.one(invalidValue(json, classOf[Object])))
      converted <- convert(a)
    } yield converted

  def value[A](value: Value[A]): ValidatedFromJson[A] = {
    value match {
      case x: XMapData[_,_,a] => {
        val kvp = kvpGroup(x)
        (j: Json) => kvp.apply(j).map(_.head)
      }
    }
  }

  def kvpGroup[H<:HList, HL<:Nat](group: KvpGroup[H,HL]): ValidatedFromJson[H] = {
    group match {
      case KvpNil => (_: Json) => Right(HNil)

      case op: KvpGroupHead[H, al, h, hl, t, tl] => {

        def children(json: Json) : Either[NonEmptyList[ExtractionError], H] = {
          val headInterpreter = kvpGroup(op.head)
          val tailInterpreter = kvpGroup(op.tail)

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headInterpreter(json).toValidated, tailInterpreter(json).toValidated)(
              (l1: h, l2: t) => {
                op.prepend.apply(l1, l2)
              })
            .andThen { l =>
              vu.validate[H](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], H]]
            }.toEither
        }

        json: Json =>
          for {
            obj <- json.asObject.toRight(NonEmptyList.one(WrongTypeError(classOf[Any], json.getClass)))
            kids <- children(json)
          } yield kids
      }

      case op: KvpSingleValueHead[h, t, tl, a, al] => {

        def children(jsonObj: JsonObject, json: Json) : Either[NonEmptyList[ExtractionError], H] = {
          val fields = jsonObj.toList
          val headInterpreter = valueDefinition(op.fieldDefinition.op)
          val tailInterpreter = kvpGroup(op.tail)
          val headValue = headInterpreter(fields.find(_._1 == op.fieldDefinition.key).map(_._2))

          val tailValue = tailInterpreter(json.obj)

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headValue.toValidated, tailValue.toValidated)((l1, l2) => {
              l1.asInstanceOf[h] :: l2.asInstanceOf[t]
            }).toEither
            .flatMap { l =>
              vu.validate(l.asInstanceOf[a], op.validations)
            }.asInstanceOf[Either[NonEmptyList[ExtractionError], H]]
        }

        json: Json =>
          for {
            jsonObj <- json.asObject.toRight(NonEmptyList.one(invalidValue(json, classOf[Object])))
            obj <- children(jsonObj, json)
          } yield obj
      }
      case op: XMapData[h,hl,b] =>
        val fromProducer = kvpGroup(op.from).asInstanceOf[ValidatedFromJson[h]]
        (json: Json) => {
          fromProducer(json).map(hl => op.fab(hl)).map(x => x :: HNil)
        }

    }
  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): ValidatedFromJsonOption[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] =>
        val applied = valueDefinition(op.valueDefinitionOp)
        (jsonOpt: Option[Json]) => jsonOpt match {
          case None => Right(None)
          case some@Some(json) => applied(some).map(Some(_))
        }


      case op: StringData =>
        required(op, _: Option[Json], _.asString).flatMap(vu.validate(_, op.validations))
      case op: IntData => required(op, _: Option[Json], _.asNumber.flatMap(_.toInt)).flatMap(vu.validate(_, op.validations))
      case op: BooleanData => required(op, _: Option[Json], _.asBoolean).flatMap(vu.validate(_, op.validations))
      case op: UuidData => {
        def convert(uuidString: String): Either[NonEmptyList[ExtractionError], UUID] = try {
          Right(UUID.fromString(uuidString))
        } catch {
          case _: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
        }
        requiredConvert(op, _: Option[Json], _.asString, convert).flatMap(vu.validate(_, op.validations))
      }

      case op@DateData(dateFormat, _, _) =>
        def convert(input: String): Either[NonEmptyList[ExtractionError], ZonedDateTime] = try {
          Right(ZonedDateTime.parse(input, dateFormat))
        } catch {
          case NonFatal(ex) => Left(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
        }
        requiredConvert(op, _: Option[Json], _.asString, convert).flatMap(vu.validate(_, op.validations))
      case ed: EitherData[a, b] =>
        val optionalA = valueDefinition(OptionalValueDefinition(ed.definitionA))
        val optionalB = valueDefinition(OptionalValueDefinition(ed.definitionB))
        json: Option[Json] => {
          optionalA(json).right.flatMap {
            case Some(a) => Right(Left(a))
            case None => {
              optionalB(json).right.flatMap {
                case Some(b) => Right(Right(b))
                case None => Left(NonEmptyList.one(RequiredData(ed)))
              }
            }
          }
        }


      case op: ListData[t,l] =>
        def traverseArray(arr: Seq[Json]) = {
          arr.map(jValue => valueDefinition(op).apply(Some(jValue)))
            .foldLeft[Either[NonEmptyList[ExtractionError], List[_]]](Right(List.empty))((b, v) => (b, v) match {
            case (Right(a), Right(i)) => Right(a :+ i)
            case (Left(a), Left(b)) => Left(a ::: b)
            case (Left(x), _) => Left(x)
            case (_, Left(x)) => Left(x)
          })
        }
        jsonOpt: Option[Json] => {
          for {
            json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
            arr <- json.asArray.toRight(NonEmptyList.one(invalidValue(json, classOf[Object])))
            result <- traverseArray(arr)
          } yield result
        }
      case op: BigDecimalFromString =>
        def convertFromString(str: String): Either[NonEmptyList[ExtractionError], BigDecimal] = {
          try {
            Right(BigDecimal(str))
          } catch {
            case ex: NumberFormatException => Left(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
          }
        }
        jsonOpt: Option[Json] =>
          requiredConvert(op, jsonOpt, _.asString, convertFromString)
            .flatMap(vu.validate(_, op.validations))
      case op: DoubleData =>
        required(op, _: Option[Json], _.asNumber.map(_.toDouble))
      case op:EnumerationStringData[a] => (jsonOpt: Option[Json]) => {
        jsonOpt.toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(RequiredData(op)))
          .flatMap(json => json.asString match {
            case Some(str) => try {
              Right(op.enumeration.withName(str).asInstanceOf[A])
            } catch {
              case ex: NoSuchElementException => Left(NonEmptyList.one(CanNotConvert(str, op.enumeration.getClass)))
            }
            case None => Left(NonEmptyList.one(invalidValue(json, classOf[BigDecimal])))
          })
      }

      case op: EnumStringData[A] => {
        def convert(str: String) = {
          op.enums.find(_.toString === str)
            .toRight(NonEmptyList.one(CanNotConvert(str, op.enums.getClass)))
        }
        jsonOpt: Option[Json] =>
          requiredConvert(op, jsonOpt, _.asString, convert)
            .flatMap(vu.validate(_, op.validations))
      }
      case op: SumTypeData[a,A] =>
        val fromProducer = valueDefinition(op.from)
        (json: Option[Json]) => {
          fromProducer.apply(json).flatMap(res => op.fab.apply(res))
        }
      case op: KvpGroupData[h,hl] => {
        val fg = kvpGroup(op.kvpGroup)
        (jsonOpt: Option[Json]) => {
          jsonOpt match {
            case Some(json) => fg(json).flatMap(res => vu.validate(res, op.validations))
            case None => Left(NonEmptyList.one(RequiredData(op)))
          }
        }
      }

    }
    result.asInstanceOf[ValidatedFromJsonOption[A]]
  }
}
