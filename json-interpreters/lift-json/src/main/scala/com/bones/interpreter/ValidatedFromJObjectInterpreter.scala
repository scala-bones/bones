package com.bones.interpreter

import java.time.ZonedDateTime
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value.{OptionalValueDefinition, ValueDefinitionOp, _}
import com.bones.validation.{ValidationUtil => vu}
import net.liftweb.json.DefaultFormats
import net.liftweb.json.JsonAST._
import shapeless.HNil

import scala.util.control.NonFatal

object ValidatedFromJObjectInterpreter {
  type ValidatedFromJObject[A] = Option[JValue] => Either[NonEmptyList[ExtractionError], A]
}

/** Compiler responsible for extracting data from JSON */
case class ValidatedFromJObjectInterpreter() {

  import ValidatedFromJObjectInterpreter._

  implicit val formats = DefaultFormats
  private def toObj(jValue: JValue) : Option[JObject] = jValue match {
    case o: JObject => Some(o)
    case _ => None
  }

  private def toArray(jValue: JValue): Option[JArray] = jValue match {
    case a: JArray => Some(a)
    case _ => None
  }

  def required[A:Manifest](op: ValueDefinitionOp[A], jsonOpt: Option[JValue], f: JValue => Option[A]): Either[NonEmptyList[ExtractionError],A] =
    for {
      json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op))).asInstanceOf[Either[NonEmptyList[ExtractionError],JValue]]
      a <- f(json).toRight(NonEmptyList.one(invalidValue(json, manifest[A].runtimeClass))).asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
    } yield a

  def requiredConvert[A:Manifest,B](op: ValueDefinitionOp[B],
                                    jsonOpt: Option[JValue],
                                    f: JValue => Option[A],
                                    convert: A => Either[NonEmptyList[ExtractionError],B]
                                   ): Either[NonEmptyList[ExtractionError],B] =
    for {
      json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
      a <- f(json).toRight(NonEmptyList.one(invalidValue(json, manifest[A].runtimeClass)))
      converted <- convert(a)
    } yield converted


  protected def invalidValue[T](jValue: JValue, expected: Class[T]): WrongTypeError[T] = {
    val invalid = jValue match {
      case JObject(_) => classOf[Object]
      case JBool(_) => classOf[Boolean]
      case JInt(_) => classOf[Int]
      case JNothing | JNull => classOf[Nothing]
      case JArray(_) => classOf[Array[_]]
      case JDouble(_) => classOf[Double]
      case JString(_) => classOf[String]
    }
    WrongTypeError(expected, invalid)
  }

  def apply[A](fgo: ValueDefinitionOp[A]): ValidatedFromJObject[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] =>
        implicit val ma = op.manifestOfOptionType
        (jsonOpt: Option[JValue]) => jsonOpt match {
          case None => Right(None)
          case some@Some(json) => apply(op.valueDefinitionOp).apply(some).map(Some(_))
        }

      case KvpNil => (_: Option[JValue]) => Right(HNil)

      case op: KvpGroupHead[A, al, h, hl, t, tl] => {

        def children(json: JValue) : Either[NonEmptyList[ExtractionError], A] = {
          implicit val mh = op.manifestOfH
          implicit val mt = op.manifestOfT
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

        jsonOpt: Option[JValue] =>
          for {
            json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
            _ <- toObj(json).toRight(NonEmptyList.one(WrongTypeError(classOf[Any], json.getClass)))
            obj <- children(json)
          } yield obj
      }

      case op: KvpSingleValueHead[h, t, tl, a, al] => {

        def children(jsonObj: JObject) : Either[NonEmptyList[ExtractionError], A] = {
          val fields = jsonObj.obj
          implicit val mt = op.manifestOfT
          implicit val mh = op.manifestOfH
          val headInterpreter = apply(op.fieldDefinition.op)
          val tailInterpreter = apply(op.tail)
          val headValue = headInterpreter(fields.find(_.name == op.fieldDefinition.key).map(_.value))

          val tailValue = tailInterpreter.apply(Some(jsonObj))

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headValue.toValidated, tailValue.toValidated)((l1, l2) => {
              l1.asInstanceOf[h] :: l2.asInstanceOf[t]
            }).toEither
            .flatMap { l =>
              vu.validate(l.asInstanceOf[a], op.validations).asInstanceOf[Either[NonEmptyList[ExtractionError], A]]
            }
        }

        (jsonOpt: Option[JValue]) =>
          for {
            json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
            jsonObj <- toObj(json).toRight(invalidValue(json, op.manifestOfResultType.runtimeClass))
            obj <- children(jsonObj)
          } yield obj

      }
      case op: StringData =>
        required(op, _: Option[JValue], _.extractOpt[String]).flatMap(vu.validate(_, op.validations))

      case op: IntData =>
        required(op, _: Option[JValue], _.extractOpt[Int]).flatMap(vu.validate(_, op.validations))
      case op: BooleanData =>
        required(op, _: Option[JValue], _.extractOpt[Boolean]).flatMap(vu.validate(_, op.validations))
      case op: UuidData =>
        def convert(uuidString: String): Either[NonEmptyList[ExtractionError], UUID] = try {
          Right(UUID.fromString(uuidString))
        } catch {
          case _: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
        }
        requiredConvert(op, _: Option[JValue], _.extractOpt[String], convert).flatMap(vu.validate(_, op.validations))


      case op@DateData(dateFormat, _, validations) =>
        def convert(input: String): Either[NonEmptyList[ExtractionError], ZonedDateTime] = try {
          Right(ZonedDateTime.parse(input, dateFormat))
        } catch {
          case NonFatal(ex) => Left(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
        }
        requiredConvert(op, _: Option[JValue], _.extractOpt[String], convert).flatMap(vu.validate(_, op.validations))

      case ed: EitherData[a, b] =>
        implicit val mr = ed.manifestOfRight
        implicit val ml = ed.manifestOfLeft
        json: Option[JValue] =>
          apply(ed.definitionA).apply(json).map(Left(_)) match {
            case l: Left[_,_] => l
            case Right(_) => apply(ed.definitionB).apply(json).map(Right(_))
          }

      case op@ListData(definition, validations) =>
        def traverseArray(arr: Seq[JValue]) = {
          arr.map(jValue => this.apply(op).apply(Some(jValue)))
            .foldLeft[Either[NonEmptyList[ExtractionError], List[_]]](Right(List.empty))((b, v) => (b, v) match {
            case (Right(a), Right(i)) => Right(a :+ i)
            case (Left(a), Left(b)) => Left(a ::: b)
            case (Left(x), _) => Left(x)
            case (_, Left(x)) => Left(x)
          })
        }
        jsonOpt: Option[JValue] => {
          for {
            json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
            arr <- toArray(json).toRight(NonEmptyList.one(invalidValue(json, op.manifestOfResultType.runtimeClass)))
            result <- traverseArray(arr.arr)
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
        jsonOpt: Option[JValue] =>
          requiredConvert(op, jsonOpt, _.extractOpt[String], convertFromString)
            .flatMap(vu.validate(_, op.validations))

      case op@ DoubleData(validations) =>
        required(op, _: Option[JValue], _.extractOpt[Double])
      case op: Convert[a,b] =>
        implicit val manifestOfInputType = op.manifestOfInputType
        jsonOpt: Option[JValue] => {
          val baseValue = apply(op.from).apply(jsonOpt)
          baseValue.flatMap(a => op.fab(a).leftMap(NonEmptyList.one)).asInstanceOf[Either[cats.data.NonEmptyList[com.bones.data.Error.ExtractionError],A]]
            .flatMap(vu.validate(_, op.validations))
        }
      case op:EnumerationStringData[a] => (jsonOpt: Option[JValue]) => {
        jsonOpt.toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(RequiredData(op)))
          .flatMap(json => json.extractOpt[String] match {
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
        jsonOpt: Option[JValue] =>
          requiredConvert(op, jsonOpt, _.extractOpt[String], convert)
            .flatMap(vu.validate(_, op.validations))
      }
      case op: XMapData[_,A] => (jsonOpt: Option[JValue]) => {
        implicit val inputManifest = op.manifestOfInputType
        implicit val outputManfiest = op.manifestOfResultType
        val fromProducer = this.apply(op.from)
        fromProducer.apply(jsonOpt).map(res => op.fab.apply(res))
      }
      case op: SumTypeData[a,A] => (json: Option[JValue]) => {
        implicit val inputTypeManifest = op.manifestOfInputType
        implicit val resultTypeManfiest = op.manifestOfResultType
        val fromProducer = this.apply(op.from)
        fromProducer.apply(json).flatMap(res => op.fab.apply(res))
      }
    }
    result.asInstanceOf[ValidatedFromJObject[A]]
  }
}