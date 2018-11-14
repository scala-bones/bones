package com.bones.circe

import java.time.ZonedDateTime
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value.{ValueDefinitionOp, _}
import cats.implicits._
import io.circe.{Json, JsonObject}
import shapeless.HNil
import com.bones.validation.{ValidationUtil => vu}

import scala.util.control.NonFatal

object ValidatedFromCirceInterpreter {
//  trait Validated[I,O]
//  case class ValidatedString() extends Validated[String,JString]
//  case class ValidatedObject[A]() extends Validated[A, ]
//
  type ValidatedFromJson[A] = Option[Json] => Either[NonEmptyList[ExtractionError], A]

  import com.bones.circe.ValidatedFromCirceInterpreter.ValidatedFromJson

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

  def optionalValueDefinition[A](op: OptionalValueDefinition[A]) : ValidatedFromJson[Option[A]] = ???

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

  def apply[A](fgo: ValueDefinitionOp[A]): ValidatedFromJson[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] =>
        (jsonOpt: Option[Json]) => jsonOpt match {
          case None => Right(None)
          case some@Some(json) => apply(op.valueDefinitionOp).apply(some).map(Some(_))
        }
      case KvpNil => (_: Option[Json]) => Right(HNil)

      case op: KvpGroupHead[A, al, h, hl, t, tl] => {

        def children(json: Json) : Either[NonEmptyList[ExtractionError], A] = {
          val headInterpreter = this.apply(op.head)
          val tailInterpreter = this.apply(op.tail)

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headInterpreter(Some(json)).toValidated, tailInterpreter(Some(json)).toValidated)(
              (l1: h, l2: t) => {
                op.prepend.apply(l1, l2)
              })
            .andThen { l =>
              vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
            }.toEither
        }

        jsonOpt: Option[Json] =>
          for {
            json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
            _ <- json.asObject.toRight(NonEmptyList.one(WrongTypeError(classOf[Any], json.getClass)))
            obj <- children(json)
          } yield obj
      }

      case op: KvpSingleValueHead[h, t, tl, a, al] => {

        def children(jsonObj: JsonObject, json: Json) : Either[NonEmptyList[ExtractionError], A] = {
          val fields = jsonObj.toList
          val headInterpreter = apply(op.fieldDefinition.op)
          val tailInterpreter = apply(op.tail)
          val headValue = headInterpreter(fields.find(_._1 == op.fieldDefinition.key).map(_._2))

          val tailValue = tailInterpreter.apply(Some(json.obj))

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headValue.toValidated, tailValue.toValidated)((l1, l2) => {
              l1.asInstanceOf[h] :: l2.asInstanceOf[t]
            }).toEither
            .flatMap { l =>
              vu.validate(l.asInstanceOf[a], op.validations)
            }.asInstanceOf[Either[NonEmptyList[ExtractionError], A]]
        }

        (jsonOpt: Option[Json]) =>
          for {
            json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
            jsonObj <- json.asObject.toRight(invalidValue(json, classOf[Object]))
            obj <- children(jsonObj, json)
          } yield obj

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
        json: Option[Json] => {
          val optionalA = OptionalValueDefinition(ed.definitionA)
          val optionalB = OptionalValueDefinition(ed.definitionB)
          apply(optionalA).apply(json).right.flatMap {
            case Some(a) => Right(Left(a))
            case None => {
              apply(optionalB).apply(json).right.flatMap {
                case Some(b) => Right(Right(b))
                case None => Left(NonEmptyList.one(RequiredData(ed)))
              }
            }
          }
        }


      case op: ListData[t,l] =>
        def traverseArray(arr: Seq[Json]) = {
          arr.map(jValue => this.apply(op).apply(Some(jValue)))
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
      case op: Convert[a,b] =>
        jsonOpt: Option[Json] => {
          val baseValue = apply(op.from).apply(jsonOpt)
          baseValue.flatMap(a => op.fab(a).leftMap(NonEmptyList.one))
            .flatMap(vu.validate(_, op.validations))
            .asInstanceOf[Either[cats.data.NonEmptyList[com.bones.data.Error.ExtractionError],A]]
        }
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
      case op: XMapData[_,A] => (jsonOpt: Option[Json]) => {
        val fromProducer = this.apply(op.from)
        fromProducer.apply(jsonOpt).map(res => op.fab.apply(res))
      }
      case op: SumTypeData[a,A] => (json: Option[Json]) => {
        val fromProducer = this.apply(op.from)
        fromProducer.apply(json).flatMap(res => op.fab.apply(res))
      }

    }
    result.asInstanceOf[ValidatedFromJson[A]]
  }
}
