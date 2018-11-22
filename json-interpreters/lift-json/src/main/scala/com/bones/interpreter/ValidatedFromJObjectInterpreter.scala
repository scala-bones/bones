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
import shapeless.{HList, HNil, Nat}

import scala.util.control.NonFatal

object ValidatedFromJObjectInterpreter {
  type ValidatedFromJObjectOpt[A] = Option[JValue] => Either[NonEmptyList[ExtractionError], A]
  type ValidatedFromJObject[A] = JValue => Either[NonEmptyList[ExtractionError], A]
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
  private def toLong(jValue: JValue): Option[Long] = jValue match {
    case a: JInt => Some(a.num.toLong)
    case _ => None
  }
  private def fromJsonToString(jValue: JValue): Option[String] = jValue match {
    case a: JString => Some(a.s)
    case _ => None
  }
  private def toDouble(jValue: JValue): Option[Double] = jValue match {
    case a: JDouble => Some(a.num)
    case _ => None
  }
  private def toBoolean(jValue: JValue): Option[Boolean] = jValue match {
    case a: JBool => Some(a.value)
    case _ => None
  }


  def required[A](op: ValueDefinitionOp[A], jsonOpt: Option[JValue], f: JValue => Option[A]): Either[NonEmptyList[ExtractionError],A] =
    for {
      json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op))).asInstanceOf[Either[NonEmptyList[ExtractionError],JValue]]
      a <- f(json).toRight(NonEmptyList.one(invalidValue(json, classOf[Object]))).asInstanceOf[Either[NonEmptyList[ExtractionError],A]]
    } yield a

  def requiredConvert[A,B](op: ValueDefinitionOp[B],
                                    jsonOpt: Option[JValue],
                                    f: JValue => Option[A],
                                    convert: A => Either[NonEmptyList[ExtractionError],B]
                                   ): Either[NonEmptyList[ExtractionError],B] =
    for {
      json <- jsonOpt.toRight(NonEmptyList.one(RequiredData(op)))
      a <- f(json).toRight(NonEmptyList.one(invalidValue(json, classOf[Object])))
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

  def dataClass[A](value: DataClass[A]): ValidatedFromJObject[A] = {
    value match {
      case x: XMapData[h,hl,a] => {
        val kvpF = kvpGroup(x.from)
        (j: JValue) => kvpF(j).map(result => x.fab(result))
      }
      case o: OptionalDataClass[a] =>
        val oF = dataClass(o.value)
        (j: JValue) => oF(j).map(Some(_))
    }
  }

  def kvpGroup[H<:HList, HL<:Nat](group: KvpGroup[H,HL]): ValidatedFromJObject[H] = {
    group match {
      case KvpNil => (_: JValue) => Right(HNil)

      case op: KvpDataClassHead[h,t,tl,out] => {
        val dcF = dataClass(op.dataClass)
        val tailF = kvpGroup(op.tail)
        (json:JValue) => {
          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(dcF(json).toValidated, tailF(json).toValidated)(
              (l1: h, l2: t) => {
                l1 :: l2 :: HNil
              }).toEither
            .flatMap { l =>
              vu.validate[out](l.asInstanceOf[out], op.validations)
            }
        }
      }

      case op: KvpGroupHead[H, al, h, hl, t, tl] => {

        def children(json: JValue) : Either[NonEmptyList[ExtractionError], H] = {
          val headInterpreter = kvpGroup(op.head)
          val tailInterpreter = kvpGroup(op.tail)

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headInterpreter(json).toValidated, tailInterpreter(json).toValidated)(
              (l1: h, l2: t) => {
                op.prepend.apply(l1, l2)
              }).toEither
            .flatMap { l =>
              vu.validate[H](l, op.validations)
            }
        }

        jValue: JValue => {
          for {
            obj <- toObj(jValue).toRight(NonEmptyList.one(WrongTypeError(classOf[Any], jValue.getClass)))
            kids <- children(obj)
          } yield kids
        }

      }

      case op: KvpSingleValueHead[h, t, tl, a] => {

        def children(jsonObj: JObject) : Either[NonEmptyList[ExtractionError], H] = {
          val fields = jsonObj.obj
          val headInterpreter = valueDefinition(op.fieldDefinition.op)
          val tailInterpreter = kvpGroup(op.tail)
          val headValue = headInterpreter(fields.find(_.name == op.fieldDefinition.key).map(_.value))

          val tailValue = tailInterpreter(jsonObj)

          Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
            .map2(headValue.toValidated, tailValue.toValidated)((l1, l2) => {
              l1.asInstanceOf[h] :: l2.asInstanceOf[t]
            }).toEither
            .flatMap { l =>
              vu.validate(l.asInstanceOf[a], op.validations).asInstanceOf[Either[NonEmptyList[ExtractionError], H]]
            }
        }

        (json: JValue) =>
          for {
            jsonObj <- toObj(json).toRight(NonEmptyList.one(invalidValue(json, classOf[Object])))
            obj <- children(jsonObj)
          } yield obj

      }
      case op: OptionalKvpGroup[h,hl] => ???
    }

  }

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): ValidatedFromJObjectOpt[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] =>
        (jsonOpt: Option[JValue]) => jsonOpt match {
          case None => Right(None)
          case some@Some(json) => valueDefinition(op.valueDefinitionOp).apply(some).map(Some(_))
        }
      case op: StringData =>
        required(op, _: Option[JValue], fromJsonToString).flatMap(vu.validate(_, op.validations))

      case op: LongData =>
        required(op, _: Option[JValue], toLong).flatMap(vu.validate(_, op.validations))
      case op: BooleanData =>
        required(op, _: Option[JValue], toBoolean).flatMap(vu.validate(_, op.validations))
      case op: UuidData =>
        def convert(uuidString: String): Either[NonEmptyList[ExtractionError], UUID] = try {
          Right(UUID.fromString(uuidString))
        } catch {
          case _: IllegalArgumentException => Left(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
        }
        requiredConvert(op, _: Option[JValue], fromJsonToString, convert).flatMap(vu.validate(_, op.validations))


      case op@DateTimeData(dateFormat, _, validations) =>
        def convert(input: String): Either[NonEmptyList[ExtractionError], ZonedDateTime] = try {
          Right(ZonedDateTime.parse(input, dateFormat))
        } catch {
          case NonFatal(ex) => Left(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
        }
        requiredConvert(op, _: Option[JValue], fromJsonToString, convert).flatMap(vu.validate(_, op.validations))

      case ed: EitherData[a, b] =>
        json: Option[JValue] => {
//          val optionalA = OptionalValueDefinition(ed.definitionA)
//          val optionalB = OptionalValueDefinition(ed.definitionB)
          valueDefinition(ed.definitionA).apply(json) match {
            case Left(errorA) =>
              //if we received a WrongTypeError we can try the other option
              val errorsRevised = errorA.toList
                .filterNot(_.isInstanceOf[WrongTypeError[_]])
              if (errorsRevised.isEmpty) {
                valueDefinition(ed.definitionB).apply(json) match {
                  case Right(b) => Right(Right(b))
                  case Left(errorB) => Left(errorB)
                }
              } else {
                Left(errorA)
              }
            case Right(a) => Right(Left(a))
          }
        }
      case op@ListData(definition, validations) =>
        def traverseArray(arr: Seq[JValue]) = {
          arr.map(jValue => valueDefinition(op).apply(Some(jValue)))
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
            arr <- toArray(json).toRight(NonEmptyList.one(invalidValue(json, classOf[List[Object]])))
            result <- traverseArray(arr.arr)
          } yield result
        }

      case op: BigDecimalData =>
        def convertFromString(str: String): Either[NonEmptyList[ExtractionError], BigDecimal] = {
          try {
            Right(BigDecimal(str))
          } catch {
            case ex: NumberFormatException => Left(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
          }
        }
        jsonOpt: Option[JValue] =>
          requiredConvert(op, jsonOpt, fromJsonToString, convertFromString)
            .flatMap(vu.validate(_, op.validations))

      case op:EnumerationStringData[a] => (jsonOpt: Option[JValue]) => {
        jsonOpt.toRight[NonEmptyList[ExtractionError]](NonEmptyList.one(RequiredData(op)))
          .flatMap(json => fromJsonToString(json) match {
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
          requiredConvert(op, jsonOpt, fromJsonToString, convert)
            .flatMap(vu.validate(_, op.validations))
      }
      case op: SumTypeData[a,A] => (json: Option[JValue]) => {
        val fromProducer = valueDefinition(op.from)
        fromProducer.apply(json).flatMap(res => op.fab.apply(res))
      }
      case op: KvpGroupData[h,hl] => {
        val fg = kvpGroup(op.kvpGroup)
        (jsonOpt: Option[JValue]) => {
          jsonOpt match {
            case Some(json) => fg(json).flatMap(res => vu.validate(res, op.validations))
            case None => Left(NonEmptyList.one(RequiredData(op)))
          }
        }
      }
      case op: KvpValueData[a] => {
        val fg = dataClass(op.value)
        (jsonOpt:Option[JValue]) => {
          jsonOpt match {
            case Some(json) => fg(json).flatMap(res => vu.validate(res, op.validations))
            case None => Left(NonEmptyList.one(RequiredData(op)))
          }
        }
      }

    }
    result.asInstanceOf[ValidatedFromJObjectOpt[A]]
  }
}