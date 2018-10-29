package com.bones.interpreter

import java.time.ZonedDateTime
import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.{SumTypeDefinition, OptionalFieldDefinition, RequiredFieldDefinition}
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.Value.{OptionalValueDefinition, ValueDefinitionOp, _}
import com.bones.validation.{ValidationUtil => vu}
import net.liftweb.json.JsonAST._
import shapeless.HNil

import scala.util.control.NonFatal

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
      case JString(_) => classOf[String]
    }
    WrongTypeError(expected, invalid)
  }

  def apply[A](fgo: ValueDefinitionOp[A]): ValidatedFromJObject[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[b] => (jValue: JValue) => {
        val result = apply(op.valueDefinitionOp)(jValue) match {
          case Valid(v) => Valid(Some(v))
          case Invalid(x) =>
            x.filterNot(_.isInstanceOf[RequiredData[b]]).toNel match {
              case Some(nel) => Invalid(nel)
              case None => Valid(None)
            }
        }
        result
      }
      case KvpNil => (_: JValue) => Valid(HNil)

      case op: KvpGroupHead[A, al, h, hl, t, tl] => (jValue: JValue) => {
        jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case obj: JObject =>
            val headInterpreter = this.apply(op.head)
            val tailInterpreter = this.apply(op.tail)

            Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
              .map2(headInterpreter(obj), tailInterpreter(obj))(
                (l1, l2) => {
                op.prepend.apply(l1, l2)
              })
              .andThen { l =>
                vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
              }
          case other  => Invalid(NonEmptyList.one(invalidValue(other, classOf[Object])))
        }
      }

      case op: KvpSingleValueHead[h, t, tl, A, al] => (jValue: JValue) => {
        val result = jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JObject(fields) => {
            val headInterpreter = apply(op.fieldDefinition.op)
            val tailInterpreter = apply(op.tail)
            val optional = op.fieldDefinition match {
              case OptionalFieldDefinition(_, _, _) => true
              case RequiredFieldDefinition(_, _, _) => false
              case SumTypeDefinition(_, _, _) => false
            }
            val headValue = fields.find(_.name == op.fieldDefinition.key.name).map(_.value) match {
              case Some(field) =>
                headInterpreter.apply(field)
                  .andThen(ex => vu.validate[h](ex.asInstanceOf[h], op.fieldDefinition.validations))
              case None =>
                if (optional) Valid(None)
                else Invalid(NonEmptyList.one(RequiredData(op)))
            }
            val tailValue = tailInterpreter.apply(jValue)
              .asInstanceOf[Validated[NonEmptyList[ExtractionError], t]]

            Applicative[({type AL[AA] = Validated[NonEmptyList[ExtractionError], AA]})#AL]
              .map2(headValue, tailValue)((l1, l2) => {

                l1.asInstanceOf[h] :: l2.asInstanceOf[t]
              })
              .andThen { l =>
                vu.validate[A](l.asInstanceOf[A], op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
              }


          }
          case other => Invalid(NonEmptyList.one(invalidValue(other, classOf[Object])))
        }
        result.asInstanceOf[Validated[cats.data.NonEmptyList[ExtractionError],A]]
      }
      case op: StringData => (jValue: JValue) => {
        val result = jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) => {
            Valid(str)
          }
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[String])))
        }
        result
      }
      case op: IntData => (jValue: JValue) => {
        val result = jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JInt(i) => {
            Valid(i.intValue())
          }
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Int])))
        }
        result
      }
      case op: BooleanData => (jValue: JValue) => {
        val result = jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JBool(b) => {
            Valid(b).asInstanceOf[Validated[NonEmptyList[ExtractionError], Boolean]]
          }
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Boolean])))
        }
        result
      }
      case op: UuidData => (jValue: JValue) => {
        def convert(uuidString: String): Validated[NonEmptyList[ExtractionError], UUID] = try {
          Valid(UUID.fromString(uuidString))
        } catch {
          case _: IllegalArgumentException => Invalid(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
        }

        jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) => convert(str)
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Boolean])))
        }
      }

      case op@DateData(dateFormat, _) => (jValue: JValue) => {
        def convert(input: String): Validated[NonEmptyList[ExtractionError], ZonedDateTime] = try {
          Valid(ZonedDateTime.parse(input, dateFormat))
        } catch {
          case NonFatal(ex) => Invalid(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
        }

        jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) => convert(str)
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[String])))
        }
      }
      case ed: EitherData[a, b] => (jValue: JValue) => {
        val result = (apply(ed.definitionA)(jValue).map(Left(_)) match {
          case Valid(bueno) => Valid(bueno)
          case Invalid(_) => {
            apply(ed.definitionB)(jValue).map(Right(_))
          }
        })
        result.asInstanceOf[Validated[NonEmptyList[ExtractionError], Either[a, b]]]
      }
      case op@ListData(definition) => (jValue: JValue) => jValue match {
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
        case other => Invalid(NonEmptyList.one(invalidValue(other, classOf[Object])))
      }

      case op: BigDecimalFromString => (jValue: JValue) => {
        def convertFromString(str: String): Validated[NonEmptyList[ExtractionError], BigDecimal] = {
          try {
            Valid(BigDecimal(str))
          } catch {
            case ex: NumberFormatException => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
          }
        }

        jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) =>
            convertFromString(str)
          case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[BigDecimal])))
        }
      }
      case op@ DoubleData() => (jValue: JValue) => jValue match {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
        case JDouble(num) => Valid(num)
        case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[Double])))
      }
      case SumTypeData(from, fab, _, _, _) => (jValue: JValue) => {
        val baseValue = apply(from).apply(jValue)
        baseValue.andThen(a => fab(a).toValidated.leftMap(NonEmptyList.one))
      }
      case EnumerationStringData(enumeration) => (jValue: JValue) => jValue match {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(fgo)))
        case JString(str) => try {
          Valid(enumeration.withName(str).asInstanceOf[A])
        } catch {
          case ex: NoSuchElementException => Invalid(NonEmptyList.one(CanNotConvert(str, enumeration.getClass)))
        }
        case x => Invalid(NonEmptyList.one(invalidValue(x, classOf[BigDecimal])))
      }

      case op: EnumStringData[a] => (jValue: JValue) => jValue match {
        case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(fgo)))
        case JString(str) =>
          op.enums.find(_.toString === str).map(_.asInstanceOf[A]).toRight(NonEmptyList.one(CanNotConvert(str, op.enums.getClass)))
            .toValidated
        case x => Invalid(NonEmptyList.one(invalidValue(x, op.manifestOfA.runtimeClass)))
      }
      case Transform(op, fba, _) => (jValue: JValue) => {
        val fromProducer = this.apply(op)
        fromProducer.apply(jValue).map(res => fba.apply(res))
      }
    }
    result.asInstanceOf[ValidatedFromJObject[A]]
  }
}