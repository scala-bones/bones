package com.bones.interpreter

import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.bones.data.Algebra._
import com.bones.data.HListAlgebra._
import com.bones.data.Key
import com.bones.json.JsonExtract
import com.bones.validation.ValidationDefinition.ValidationOp
import com.bones.validation.{ValidationUtil => vu}
import net.liftweb.json.JsonAST._
import shapeless.HNil

import scala.util.control.NonFatal


object ExtractionInterpreter {

  /** Error Case */
  sealed trait ExtractionError

  /**
    * Used to indicate a validation error.
    * @param failurePoint The extraction op where the error failed.
    * @param input The input, if available.
    * @tparam T Type that was validated.
    */
  case class ValidationError[T](failurePoint: ValidationOp[T], input: T) extends ExtractionError
  case class WrongTypeError[T](expectedType: Class[T], providedType: Class[_]) extends ExtractionError
  case class CanNotConvert[A,T](input: A, toType: Class[T]) extends ExtractionError
  case class RequiredData[A](dataDefinitionOp: DataDefinitionOp[A]) extends ExtractionError
  case class FieldError[A](key: Key, errors: NonEmptyList[ExtractionError]) extends ExtractionError


  type ExtractionErrors = NonEmptyList[ExtractionError]

  type ValidationResult[T] = Validated[ExtractionError,T]
  type ValidationResultNel[T] = Validated[NonEmptyList[ExtractionError], T]

  type ValidateFromProducer[A] = JsonExtract => ValidationResultNel[A]
  implicit def fromProducerApp = new Applicative[ValidateFromProducer] {
    override def pure[A](x: A): ValidateFromProducer[A] = json => Valid(x)
    override def ap[A, B](ff: ValidateFromProducer[A => B])(fa: ValidateFromProducer[A]): ValidateFromProducer[B] =
      jsonProducer => {
        val f = ff(jsonProducer)
        val a = fa(jsonProducer)
        (f,a).mapN( (fab, a) => fab.apply(a))
      }
  }

  /** Compiler responsible for extracting data from JSON */
  case class DefaultExtractInterpreter() extends cats.arrow.FunctionK[DataDefinitionOp, ValidateFromProducer] {
    def apply[A](fgo: DataDefinitionOp[A]): ValidateFromProducer[A] = jsonProducer =>
      fgo match {

        case op: OptionalDataDefinition[b] => {
          this(op.dataDefinitionOp)(jsonProducer) match {
            case Valid(v) => Valid(Some(v)).asInstanceOf[ValidationResultNel[A]]
            case Invalid(x) => {
              x.filterNot(_.isInstanceOf[RequiredData[b]]).toNel match {
                case Some(nel) => Invalid(nel)
                case None => Valid(None).asInstanceOf[ValidationResultNel[A]]
              }
            }
          }
        }
        case op: HListPrependN[A,p,s] => {

          jsonProducer.extractObject.leftMap(NonEmptyList.one).andThen {
            case Some(producer) => {
              val m1 = this.apply(op.prefix)
              val m2 = this.apply(op.suffix)
              (m1(producer), m2(producer))
                .mapN( (l1, l2) => op.prepend.apply(l1 :: l2 :: HNil) )
                .asInstanceOf[ValidationResultNel[A]]
            }
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }.andThen { l =>
            vu.validate[A](l, op.validations).asInstanceOf[ValidationResultNel[A]]
          }
        }
        case op: HMember[a] => {
          jsonProducer.extractObject.leftMap(NonEmptyList.one).andThen {
            case Some(producer) =>  {
              val r1 = this(op.op1.op)
              vu.pv(producer, op.op1, r1).map(_ :: HNil)
                .asInstanceOf[ValidationResultNel[A]]

            }
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }.andThen { l =>
            vu.validate[A](l, op.validations).asInstanceOf[ValidationResultNel[A]]
          }
        }

        case op: StringData => {
          jsonProducer.extractString.leftMap(NonEmptyList.one).andThen {
            case Some(e) => Valid(e).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }

        }
        case op: IntData => {
          jsonProducer.extractInt.leftMap(NonEmptyList.one).andThen {
            case Some(e) => Valid(e).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }
        }
        case op: BooleanData => {
          jsonProducer.extractBool.leftMap(NonEmptyList.one) andThen {
            case Some(x) => Valid(x).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }
        }
        case op: UuidData => {
          def convert(uuidString: String): Validated[ExtractionErrors, UUID] = try {
            Valid(UUID.fromString(uuidString))
          } catch {
            case _: IllegalArgumentException => Invalid(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
          }

          jsonProducer.extractString.leftMap(NonEmptyList.one).andThen {
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
            case Some(str) => convert(str).asInstanceOf[ValidationResultNel[A]]
          }

        }
        case op @ DateData(dateFormat, _) => {
          jsonProducer.extractString.leftMap(NonEmptyList.one).andThen {
            case Some(str) => try {
              Valid(dateFormat.parseObject(str).asInstanceOf[Date]).asInstanceOf[ValidationResultNel[A]]
            } catch {
              case NonFatal(ex) => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[Date])))
            }
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }

        }
        case EitherData(a,b) => {
          (apply(a)(jsonProducer).map(Left(_)) match {
            case Valid(bueno) => Valid(bueno)
            case Invalid(_) => {
              apply(b)(jsonProducer)
            }
          }).asInstanceOf[ValidationResultNel[A]]
        }
        case op @ ListData(definition) => {
          jsonProducer.extractList.leftMap(NonEmptyList.one).andThen {
            case Some(list) =>
              list.map(producer => {
                apply(definition)(producer)
              }).sequence.asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }
        }
        case op: BigDecimalFromString => {
          def convertFromString(str: String) : Validated[ExtractionErrors, BigDecimal] = {
            try {
              Valid(BigDecimal(str))
            } catch {
              case ex: NumberFormatException => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
            }
          }

          jsonProducer.extractString.leftMap(NonEmptyList.one).andThen {
            case Some(str) => convertFromString(str).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }
        }
        case ConversionData(from, fab, _, _) => {
          val baseValue = apply(from).apply(jsonProducer)
          baseValue.andThen(a => fab(a).toValidated.leftMap(NonEmptyList.one))
        }
        case op @ EnumeratedStringData(enumeration) => {
          jsonProducer.extractString.leftMap(NonEmptyList.one).andThen {
            case Some(str) => try {
              Valid(enumeration.withName(str).asInstanceOf[A])
            } catch {
              case ex: NoSuchElementException => Invalid(NonEmptyList.one(CanNotConvert(str, enumeration.getClass)))
            }
            case None => Invalid(NonEmptyList.one(RequiredData(op)))
          }
        }
        case Transform(op, _, fba) => {
          val fromProducer = this.apply(op)
          fromProducer.apply(jsonProducer).map(res => fba.apply(res))
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
