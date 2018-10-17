package com.bones.circe

import java.time.ZonedDateTime
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.{ConversionFieldDefinition, OptionalFieldDefinition, RequiredFieldDefinition}
import com.bones.data.Value._
import cats.implicits._
import io.circe.Json
import shapeless.HNil
import com.bones.validation.{ValidationUtil => vu}

import scala.util.control.NonFatal

object ValidatedFromCirceInterpreter {
  type ValidatedFromJson[A] = Json => Validated[NonEmptyList[ExtractionError], A]

}
case class ValidatedFromCirceInterpreter() {

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

  def apply[A](fgo: ValueDefinitionOp[A]): ValidatedFromJson[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[b] => (json: Json) => {
        val result = apply(op.valueDefinitionOp)(json) match {
          case Valid(v) => Valid(Some(v))
          case Invalid(x) =>
            x.filterNot(_.isInstanceOf[RequiredData[b]]).toNel match {
              case Some(nel) => Invalid(nel)
              case None => Valid(None)
            }
        }
        result
      }
      case KvpNil => (_: Json) => Valid(HNil)

      case op: KvpGroupHead[A, al, h, hl, t, tl] => (json: Json) => {
        json.asObject match {
          case None => Invalid(NonEmptyList.one(RequiredData(op)))
          case Some(obj) =>
            val headInterpreter = this.apply(op.head)
            val tailInterpreter = this.apply(op.tail)

            Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
              .map2(headInterpreter(json), tailInterpreter(json))(
                (l1, l2) => {
                  op.prepend.apply(l1, l2)
                })
              .andThen { l =>
                vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
              }
        }
      }

      case op: KvpSingleValueHead[h, t, tl, A, al] => (json: Json) => {
        val result = json.asObject match {
          case None => Invalid(NonEmptyList.one(RequiredData(op)))
          case Some(obj) => {
            val fields = obj.toList
            val headInterpreter = apply(op.fieldDefinition.op)
            val tailInterpreter = apply(op.tail)
            val optional = op.fieldDefinition match {
              case OptionalFieldDefinition(_, _, _) => true
              case RequiredFieldDefinition(_, _, _) => false
              case ConversionFieldDefinition(_, _, _) => false
            }
            val headValue = fields.find(_._1 == op.fieldDefinition.key.name).map(_._2) match {
              case Some(field) =>
                headInterpreter.apply(field)
                  .andThen(ex => vu.validate[A](ex.asInstanceOf[A], op.validations))
              case None =>
                if (optional) Valid(None)
                else Invalid(NonEmptyList.one(RequiredData(op)))
            }
            val tailValue = tailInterpreter.apply(json)
              .asInstanceOf[Validated[NonEmptyList[ExtractionError], t]]

            Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
              .map2(headValue, tailValue)((l1, l2) => {
                l1.asInstanceOf[h] :: l2.asInstanceOf[t]
              })
              .andThen { l =>
                vu.validate[A](l.asInstanceOf[A], op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
              }


          }
        }
        result.asInstanceOf[Validated[cats.data.NonEmptyList[ExtractionError],A]]
      }
      case op: StringData => (json: Json) => {
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        }
        json.asString match {
          case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[String])))
          case Some(str) => {
            Valid(str)
          }
        }
      }
      case op: IntData => (json: Json) => {
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        } else {
          json.asNumber.flatMap(_.toInt) match {
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[Int])))
            case Some(i) => Valid(i)
          }
        }
      }
      case op: BooleanData => (json: Json) => {
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        } else {
          json.asBoolean match {
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[Boolean])))
            case Some(b) => Valid(b).asInstanceOf[Validated[NonEmptyList[ExtractionError], Boolean]]
          }
        }
      }
      case op: UuidData => (json: Json) => {
        def convert(uuidString: String): Validated[NonEmptyList[ExtractionError], UUID] = try {
          Valid(UUID.fromString(uuidString))
        } catch {
          case _: IllegalArgumentException => Invalid(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
        }
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        } else {
          json.asString match {
            case Some(str) => convert(str)
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[Boolean])))
          }
        }
      }

      case op@DateData(dateFormat, _) => (json: Json) => {
        def convert(input: String): Validated[NonEmptyList[ExtractionError], ZonedDateTime] = try {
          Valid(ZonedDateTime.parse(input, dateFormat))
        } catch {
          case NonFatal(ex) => Invalid(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
        }
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        } else {
          json.asString match {
            case Some(str) => convert(str)
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[String])))
          }
        }
      }
      case ed: EitherData[a, b] => (json: Json) => {
        val result = (apply(ed.definitionA)(json).map(Left(_)) match {
          case Valid(bueno) => Valid(bueno)
          case Invalid(_) => {
            apply(ed.definitionB)(json).map(Right(_))
          }
        })
        result.asInstanceOf[Validated[NonEmptyList[ExtractionError], Either[a, b]]]
      }
      case op@ListData(definition) => (json: Json) => {
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        } else {
          json.asArray match {
            case Some(arr) =>
              arr.map(jValue => this.apply(op)(jValue))
                .foldLeft[Validated[NonEmptyList[ExtractionError], List[_]]](Valid(List.empty))((b, v) => (b, v) match {
                case (Valid(a), Valid(i)) => Valid(a :+ i)
                case (Invalid(a), Invalid(b)) => Invalid(a ::: b)
                case (Invalid(x), _) => Invalid(x)
                case (_, Invalid(x)) => Invalid(x)
              }).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[Array[_]])))
          }
        }
      }
      case op: BigDecimalFromString => (json: Json) => {
        def convertFromString(str: String): Validated[NonEmptyList[ExtractionError], BigDecimal] = {
          try {
            Valid(BigDecimal(str))
          } catch {
            case ex: NumberFormatException => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
          }
        }
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(op)))
        } else {
          json.asString match {
            case Some(str) => convertFromString(str)
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[BigDecimal])))
          }
        }
      }
      case ConversionData(from, fab, _, _) => (json: Json) => {
        val baseValue = apply(from).apply(json)
        baseValue.andThen(a => fab(a).toValidated.leftMap(NonEmptyList.one))
      }
      case EnumerationStringData(enumeration) => (json: Json) => {
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(fgo)))
        } else {
          json.asString match {
            case Some(str) => try {
              Valid(enumeration.withName(str).asInstanceOf[A])
            } catch {
              case ex: NoSuchElementException => Invalid(NonEmptyList.one(CanNotConvert(str, enumeration.getClass)))
            }
            case None => Invalid(NonEmptyList.one(invalidValue(json, classOf[BigDecimal])))
          }
        }
      }

      case op: EnumStringData[a] => (json: Json) => {
        if (json.isNull) {
          Invalid(NonEmptyList.one(RequiredData(fgo)))
        } else {
          json.asString match {
            case Some(str) =>
              op.enums.find(_.toString === str).map(_.asInstanceOf[A])
                .toRight(NonEmptyList.one(CanNotConvert(str, op.enums.getClass)))
                .toValidated
            case None => Invalid(NonEmptyList.one(invalidValue(json, op.manifestOfA.runtimeClass)))
          }
        }
      }
      case Transform(op, _, fba) => (json: Json) => {
        val fromProducer = this.apply(op)
        fromProducer.apply(json).map(res => fba.apply(res))
      }
    }
    result.asInstanceOf[ValidatedFromJson[A]]
  }
}
