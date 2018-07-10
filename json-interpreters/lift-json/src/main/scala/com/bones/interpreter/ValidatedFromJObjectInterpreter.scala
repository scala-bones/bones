package com.bones.interpreter

import java.time.{LocalDate, ZonedDateTime}
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import com.bones.data.Algebra._
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.HListAlgebra.{HListPrependN, HMember}
import com.bones.data.{ConversionFieldDefinition, OptionalFieldDefinition, RequiredFieldDefinition}
import net.liftweb.json.JsonAST._
import shapeless.HNil

import scala.util.control.NonFatal
import com.bones.validation.{ValidationUtil => vu}

object ValidatedFromJObjectInterpreter {
  type ValidatedFromJObject[A] = JValue => Validated[NonEmptyList[ExtractionError], A]
}

/** Compiler responsible for extracting data from JSON */
case class ValidatedFromJObjectInterpreter() {

  import ValidatedFromJObjectInterpreter._

  protected def invalidValue[T](jValue: JValue, expected: Class[T]): WrongTypeError[T] = {
    val invalid = jValue match {
      case JObject(_) => classOf[Object]
      case JBool(_) => classOf[Boolean]
      case JInt(_) => classOf[Int]
      case JNothing | JNull => classOf[Nothing]
      case JArray(_) => classOf[Array[_]]
      case JDouble(_) => classOf[Double]
      case JField(_, f) => classOf[Nothing]
      case JString(_) => classOf[String]
    }
    WrongTypeError(expected, invalid)
  }

  def apply[A](fgo: DataDefinitionOp[A]): ValidatedFromJObject[A] =
    fgo match {

      case op: OptionalDataDefinition[b] => jValue =>
        this(op.dataDefinitionOp)(jValue) match {
          case Valid(v) => Valid(Some(v)).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
          case Invalid(x) =>
            x.filterNot(_.isInstanceOf[RequiredData[b]]).toNel match {
              case Some(nel) => Invalid(nel)
              case None => Valid(None).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
            }
        }

      case op: HListPrependN[A, p, s] => jValue => {
        jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case obj: JObject =>
            val m1 = this.apply(op.prefix)
            val m2 = this.apply(op.suffix)

            Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
              .map2(
                m1(obj),
                m2(obj)
              )((l1, l2) => {
                val input = l1 :: l2 :: HNil
                op.prepend.apply(input)
              })
              .andThen { l =>
                vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
              }

            //              (m1(obj), m2(obj)).mapN( (l1, l2) => op.prepend.apply(l1 :: l2 :: HNil) )
            //                .asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            //                .andThen { l =>
            //                  vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            //                }
          case x => Invalid(NonEmptyList.one(WrongTypeError(classOf[JObject], x.getClass)))
        }
      }
      case op: PrependDataDefinition[a,b] => jValue => {
        jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case obj: JObject => {
            val a = this.apply(op.a)
            val b = this.apply(op.b)
            Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
              .map2(
                a(obj),
                b(obj)
              )((l1, l2) => {
                l1 :: l2 :: HNil
//                op.prepend.apply(input)
              })
//              .andThen { l =>
//                vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
//              }
          }
        }
      }
      case op: HMember[a] => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
        case JObject(fields) => {
          val r1 = this (op.op1.op)
          val optional = op.op1 match {
            case OptionalFieldDefinition(_, _, _) => true
            case RequiredFieldDefinition(_, _, _) => false
            case ConversionFieldDefinition(_, _, _) => false
          }
          val result = fields.find(_.name == op.op1.key.name)
            .headOption
            .map(_.value) match {
            case Some(field) => {
              r1.apply(field).andThen(ex => {
                vu.validate[a](ex, op.op1.validations)
              })
            }
            case None => {
              if (optional) Valid(None)
              else Invalid(NonEmptyList.one(RequiredData(op)))
            }
          }
          result.map(_ :: HNil)
            .asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
        }
      }

      case op: StringData => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
        case JString(str) => {
          Valid(str).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
        }
        case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[String])))
      }
      case op: IntData => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
        case JInt(i) => {
          Valid(i.intValue()).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
        }
        case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Int])))
      }
      case op: BooleanData => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
        case JBool(b) => {
          Valid(b).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
        }
        case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Boolean])))
      }
      case op: UuidData => {
        def convert(uuidString: String): Validated[NonEmptyList[ExtractionError], UUID] = try {
          Valid(UUID.fromString(uuidString))
        } catch {
          case _: IllegalArgumentException => Invalid(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
        }

        {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) =>
            convert(str).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Boolean])))
        }
      }
      case op@DateData(dateFormat, _) => {
        def convert(input: String): Validated[NonEmptyList[ExtractionError], ZonedDateTime] = try {
          Valid(ZonedDateTime.parse(input, dateFormat))
        } catch {
          case NonFatal(ex) => Invalid(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
        }

        {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) => convert(str).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[String])))
        }
      }
      case EitherData(a, b) => jValue => {
        (apply(a)(jValue).map(Left(_)) match {
          case Valid(bueno) => Valid(bueno)
          case Invalid(_) => {
            apply(b)(jValue)
          }
        }).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
      }
      case op@ListData(definition) => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
        case JArray(arr) => {
          arr.map(jValue => this.apply(op)(jValue))
            .foldLeft[Validated[NonEmptyList[ExtractionError], List[_]]](Valid(List.empty))((b, v) => (b, v) match {
            case (Valid(a), Valid(i)) => Valid(a :+ i)
            case (Invalid(a), Invalid(b)) => Invalid(a ::: b)
            case (Invalid(x), _) => Invalid(x)
            case (_, Invalid(x)) => Invalid(x)
          }).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
        }
      }

      case op: BigDecimalFromString => {
        def convertFromString(str: String): Validated[NonEmptyList[ExtractionError], BigDecimal] = {
          try {
            Valid(BigDecimal(str))
          } catch {
            case ex: NumberFormatException => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
          }
        }

        {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) =>
            convertFromString(str).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[BigDecimal])))
        }
      }
      case ConversionData(from, fab, _, _) => jValue => {
        val baseValue = apply(from).apply(jValue)
        baseValue.andThen(a => fab(a).toValidated.leftMap(NonEmptyList.one))
      }
      case EnumerationStringData(enumeration) => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(fgo)))
        case JString(str) => try {
          Valid(enumeration.withName(str).asInstanceOf[A])
        } catch {
          case ex: NoSuchElementException => Invalid(NonEmptyList.one(CanNotConvert(str, enumeration.getClass)))
        }
        case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[BigDecimal])))
      }

      case op: EnumStringData[a] => {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(fgo)))
        case JString(str) =>
          op.enums.find(_.toString === str).map(_.asInstanceOf[A]).toRight(NonEmptyList.one(CanNotConvert(str, op.enums.getClass)))
            .toValidated
      }
      case Transform(op, _, fba) => jValue => {
        val fromProducer = this.apply(op)
        fromProducer.apply(jValue).map(res => fba.apply(res))
      }
    }
}