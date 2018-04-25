package com.bones.interpreter

import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.Algebra._
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredData, WrongTypeError}
import com.bones.data.HListAlgebra._
import com.bones.validation.{ValidationUtil => vu}
import net.liftweb.json.JsonAST._
import shapeless.HNil

import scala.util.control.NonFatal


object ExtractionInterpreter {


  type ValidateFromJObject[A] = JValue => Validated[NonEmptyList[ExtractionError],A]

  implicit def fromProducerApp = new Applicative[ValidateFromJObject] {
    override def pure[A](x: A): ValidateFromJObject[A] = json => Valid(x)
    override def ap[A, B](ff: ValidateFromJObject[A => B])(fa: ValidateFromJObject[A]): ValidateFromJObject[B] =
      jsonProducer => {
        val f = ff(jsonProducer)
        val a = fa(jsonProducer)
        (f,a).mapN( (fab, a) => fab.apply(a))
      }
  }

  def invalidValue[T](jValue: JValue, expected: Class[T]): WrongTypeError[T] = {
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

  /** Compiler responsible for extracting data from JSON */
  case class DefaultExtractInterpreter() extends cats.arrow.FunctionK[DataDefinitionOp, ValidateFromJObject] {
    def apply[A](fgo: DataDefinitionOp[A]): ValidateFromJObject[A] = jValue =>
      fgo match {

        case op: OptionalDataDefinition[b] => {
          this(op.dataDefinitionOp)(jValue) match {
            case Valid(v) => Valid(Some(v)).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            case Invalid(x) => {
              x.filterNot(_.isInstanceOf[RequiredData[b]]).toNel match {
                case Some(nel) => Invalid(nel)
                case None => Valid(None).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
              }
            }
          }
        }

        case op: HListPrependN[A,p,s] => {

          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case obj: JObject => {
              val m1 = this.apply(op.prefix)
              val m2 = this.apply(op.suffix)
              (m1(obj), m2(obj))
                .mapN( (l1, l2) => op.prepend.apply(l1 :: l2 :: HNil) )
                .asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
                .andThen { l =>
                  vu.validate[A](l, op.validations).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
                }
            }
            case x => Invalid(NonEmptyList.one(WrongTypeError(classOf[JObject], x.getClass)))
          }
        }
        case op: HMember[a] => {
          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case JObject(fields) => {
              val r1 = this(op.op1.op)
              val result = fields.find(_.name == op.op1.key.name)
                .headOption
                .map(_.value) match {
                  case Some(field) => r1.apply(field)
                  case None => Invalid(NonEmptyList.one(RequiredData(op)))
                }
              result.andThen(input => {
                vu.validate(input, op.op1.validations)
              }).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]

            }
          }
        }

        case op: StringData => jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JString(str) => {
            Valid(str).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
          }
          case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[String])))
        }
        case op: IntData => jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JInt(i) => {
            Valid(i).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
          }
          case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[Int])))
        }
        case op: BooleanData => jValue match {
          case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
          case JBool(b) => {
            Valid(b).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
          }
          case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[Boolean])))
        }
        case op: UuidData => {
          def convert(uuidString: String): Validated[NonEmptyList[ExtractionError], UUID] = try {
            Valid(UUID.fromString(uuidString))
          } catch {
            case _: IllegalArgumentException => Invalid(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
          }

          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case JString(str) =>
              convert(str).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[Boolean])))
          }
        }
        case op @ DateData(dateFormat, _) => {
          def convert(input: String) : Validated[NonEmptyList[ExtractionError],Date] = try {
            Valid(dateFormat.parseObject(input).asInstanceOf[Date])
          } catch {
            case NonFatal(ex) => Invalid(NonEmptyList.one(CanNotConvert(input, classOf[Date])))
          }
          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case JString(str) => convert(str).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[String])))
          }
        }
        case EitherData(a,b) => {
          (apply(a)(jValue).map(Left(_)) match {
            case Valid(bueno) => Valid(bueno)
            case Invalid(_) => {
              apply(b)(jValue)
            }
          }).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
        }
        case op @ ListData(definition) => {
          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case JArray(arr) => {
              arr.map(jValue => this.apply(op)(jValue))
                .sequence.asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            }
          }
        }
        case op: BigDecimalFromString => {
          def convertFromString(str: String) : Validated[NonEmptyList[ExtractionError], BigDecimal] = {
            try {
              Valid(BigDecimal(str))
            } catch {
              case ex: NumberFormatException => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
            }
          }

          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case JString(str) => {
              convertFromString(str).asInstanceOf[Validated[NonEmptyList[ExtractionError],A]]
            }
            case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[BigDecimal])))
          }
        }
        case ConversionData(from, fab, _, _) => {
          val baseValue = apply(from).apply(jValue)
          baseValue.andThen(a => fab(a).toValidated.leftMap(NonEmptyList.one))
        }
        case op @ EnumeratedStringData(enumeration) => {
          jValue match {
            case JNull | JNothing => Invalid(NonEmptyList.one(RequiredData(op)))
            case JString(str) => try {
              Valid(enumeration.withName(str).asInstanceOf[A])
            } catch {
              case ex: NoSuchElementException => Invalid(NonEmptyList.one(CanNotConvert(str, enumeration.getClass)))
            }
            case x => Invalid(NonEmptyList.one(invalidValue(jValue, classOf[BigDecimal])))
          }
        }
        case Transform(op, _, fba) => {
          val fromProducer = this.apply(op)
          fromProducer.apply(jValue).map(res => fba.apply(res))
        }
      }
  }

}

object EncoderInterpreter {


  type Encode[A] = A => JValue

  /** FunctionK requires the G[_] type parameter to be an Applicative.  I'm not sure why because this is never called here.
    * Should probably figure out why since I'm sure I'm breaking some golden rule here.
    **/
  implicit def validateAndEncodeFreeAp = new  Applicative[Encode] {
    def pure[A](x: A): Encode[A] = {
      a => JNothing
    }

    def ap[A, B](ff: Encode[A => B])(fa: Encode[A]): Encode[B] = {
      ??? // exactly, ???
    }
  }


  case class DefaultEncoderInterpreter() extends cats.arrow.FunctionK[DataDefinitionOp, Encode] {

    def apply[A](fgo: DataDefinitionOp[A]): Encode[A] = (input: A) =>
      fgo match {
        case op: OptionalDataDefinition[b] => {
          input match {
            case Some(x) => apply(op.dataDefinitionOp).apply(x)
            case None => JNothing
          }
        }
        case op: HListPrependN[A,p,s] => {
            val l = op.split(input)
            val m1 = this.apply(op.prefix).apply(l.head)
            val m2 = this.apply(op.suffix).apply(l.tail.head)
            JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
        }
        case op: HMember[a] => {
          import shapeless.::
          val res1 = this(op.op1.op)(input.asInstanceOf[a :: HNil].head)
          JObject(List(JField(op.op1.key.name, res1)))
        }
        case ob: BooleanData => JBool(input.asInstanceOf[Boolean])
        case rs: StringData => JString(input.asInstanceOf[String])
        case ri: IntData => JInt(input.asInstanceOf[Int])
        case uu: UuidData => JString(input.toString)
        case DateData(format, _) => JString(format.format(input.asInstanceOf[Date]))
        case bd: BigDecimalFromString => JString(input.toString)
        case dd: DoubleData => JDouble(input.asInstanceOf[Double])
        case ListData(definition) => {
          val f = apply(definition)
          JArray(input.asInstanceOf[List[A]].map(i => f(i)))
        }
        case EitherData(aDefinition, bDefinition) => {
          input match {
            case Left(aInput) => apply(aDefinition)(aInput)
            case Right(bInput) => apply(bDefinition)(bInput)
          }
        }
        case ConversionData(from, _, fba, _) => {
          val encoder = apply(from)
          val outputValue = fba(input)
          encoder.apply(outputValue)
        }
        case EnumeratedStringData(enumeration) => JString(input.toString)
        case Transform(op, fab, _) => {
          val b = fab(input)
          apply(op).apply(b)
        }

      }
  }

}
