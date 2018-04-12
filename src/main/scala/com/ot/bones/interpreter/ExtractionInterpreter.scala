package com.ot.bones.interpreter

import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.ot.bones.data.Algebra._
import com.ot.bones.data.Key
import com.ot.bones.data.ToHList.{BaseHListDef, HListAppend2, HListAppend3}
import com.ot.bones.interpreter.ExtractionInterpreter.ValidationResultNel
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import net.liftweb.json.JsonAST._

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
  case class RequiredObjectError() extends ExtractionError
//  case class MultiKeyError[T](keys: NonEmptyList[Key], failurePoint: ExtractionOp[T]) extends ExtractionError


  type ExtractionErrors = NonEmptyList[ExtractionError]

  type ValidationResult[T] = Validated[ExtractionError,T]
  type ValidationResultNel[T] = Validated[NonEmptyList[ExtractionError], T]






  trait StringProducer {
    def produceString: Validated[WrongTypeError[String], Option[String]]
  }
  trait IntProducer {
    def produceInt: Validated[WrongTypeError[Int], Option[Int]]
  }
  trait BoolProducer {
    def produceBool: Validated[WrongTypeError[Boolean], Option[Boolean]]
  }
  trait DoubleProducer {
    def produceDouble: Validated[WrongTypeError[Double], Option[Double]]
  }
  trait ObjectProducer {
    def produceObject: Validated[WrongTypeError[JsonProducer], Option[JsonProducer]]
  }
  trait ListProducer {
    def produceList: Validated[WrongTypeError[List[_]], Option[List[JsonProducer]]]
  }
  trait JsonKeyResolver {
    def child(key: Key): JsonProducer
  }

  /** Simple little interface around a Json library.
    * I believe it may be best to actually write an extractor
    * to and from your preferred library instead of extending JsonProducer, however this is here how I first implemented this,
    * so I'll keep this around for now.
    **/
  abstract class JsonProducer extends StringProducer with IntProducer with BoolProducer with DoubleProducer
    with ObjectProducer with ListProducer with JsonKeyResolver


  type ValidateFromProducer[A] = JsonProducer => ValidationResultNel[A]
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

        case op: HListAppend3[l1, l2, l3, o2, o3] => op.extractMembers(this)(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        case op: HListAppend2[l1, l2, out] => op.extractMembers(this)(jsonProducer).asInstanceOf[ValidationResultNel[A]]

        case op: OptionalDataDefinition[b] => {
          this(op.dataDefinitionOp)(jsonProducer) match {
            case Valid(v) => Valid(Some(v)).asInstanceOf[ValidationResultNel[A]]
            case Invalid(x) => {
              x.filterNot(_.isInstanceOf[RequiredObjectError]).toNel match {
                case Some(nel) => Invalid(nel)
                case None => Valid(None).asInstanceOf[ValidationResultNel[A]]
              }
            }
          }
        }

        case op: BaseHListDef[a] => {
          op.extract(jsonProducer, this).asInstanceOf[ValidationResultNel[A]]
        }
        case op: StringData => {
          jsonProducer.produceString.leftMap(NonEmptyList.one).andThen {
            case Some(e) => Valid(e).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
          }

        }
        case op: IntData => {
          jsonProducer.produceInt.leftMap(NonEmptyList.one).andThen {
            case Some(e) => Valid(e).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
          }
        }
        case op: BooleanData => {
          jsonProducer.produceBool.leftMap(NonEmptyList.one) andThen {
            case Some(x) => Valid(x).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
          }
        }
        case op: UuidData => {
          def convert(uuidString: String): Validated[ExtractionErrors, UUID] = try {
            Valid(UUID.fromString(uuidString))
          } catch {
            case _: IllegalArgumentException => Invalid(NonEmptyList.one(CanNotConvert(uuidString, classOf[UUID])))
          }

          jsonProducer.produceString.leftMap(NonEmptyList.one).andThen {
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
            case Some(str) => convert(str).asInstanceOf[ValidationResultNel[A]]
          }

        }
        case DateData(dateFormat, _) => {
          jsonProducer.produceString.leftMap(NonEmptyList.one).andThen {
            case Some(str) => try {
              Valid(dateFormat.parseObject(str).asInstanceOf[Date]).asInstanceOf[ValidationResultNel[A]]
            } catch {
              case NonFatal(ex) => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[Date])))
            }
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
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
        case ListData(definition) => {
          jsonProducer.produceList.leftMap(NonEmptyList.one).andThen {
            case Some(list) =>
              list.map(producer => {
                apply(definition)(producer)
              }).sequence.asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
          }
        }
        case BigDecimalFromString() => {
          def convertFromString(str: String) : Validated[ExtractionErrors, BigDecimal] = {
            try {
              Valid(BigDecimal(str))
            } catch {
              case ex: NumberFormatException => Invalid(NonEmptyList.one(CanNotConvert(str, classOf[BigDecimal])))
            }
          }

          jsonProducer.produceString.leftMap(NonEmptyList.one).andThen {
            case Some(str) => convertFromString(str).asInstanceOf[ValidationResultNel[A]]
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
          }
        }
        case ConversionData(from, fab, _, _) => {
          val baseValue = apply(from).apply(jsonProducer)
          baseValue.andThen(a => fab(a).leftMap(NonEmptyList.one))
        }
        case EnumeratedStringData(enumeration) => {
          jsonProducer.produceString.leftMap(NonEmptyList.one).andThen {
            case Some(str) => try {
              Valid(enumeration.withName(str).asInstanceOf[A])
            } catch {
              case ex: NoSuchElementException => Invalid(NonEmptyList.one(CanNotConvert(str, enumeration.getClass)))
            }
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
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
        case op: BaseHListDef[A] => {
          op.encodeMembers(this).apply(input)
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
        case op: HListAppend3[l1, l2, l3, o2, o3] => op.encodeMembers(this).apply(input.asInstanceOf[o3])
        case op: HListAppend2[l1, l2, out] => op.encodeMembers(this).apply(input.asInstanceOf[out])

      }
  }

}
