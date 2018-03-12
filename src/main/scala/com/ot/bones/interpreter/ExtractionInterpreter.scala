package com.ot.bones.interpreter

import java.util.{Date, UUID}

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.data.Algebra._
import com.ot.bones.data.Key
import com.ot.bones.data.ToHList.ToHListDataDefinitionOp
import com.ot.bones.interpreter.ExtractionInterpreter.ValidationResultNel
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import net.liftweb.json.JsonAST._
import shapeless.HList

import scala.util.control.NonFatal


object ExtractionInterpreter {

  /** Error Case */
  trait ExtractionError {
//    def keys: NonEmptyList[Key]
  }

  /**
    * Used to indicate a validation error.
    * @param failurePoint The extraction op where the error failed.
    * @param input The input, if available.
    * @tparam T Type that was validated.
    */
  case class ValidationError[T](failurePoint: ValidationOp[T], input: T) extends ExtractionError {
//    val keys = NonEmptyList.one(key)
  }
  case class WrongTypeError[T](expectedType: Class[T], providedType: Class[_]) extends ExtractionError {
//    val keys = NonEmptyList.one(key)
  }
  case class ConversionError[A,T](input: A, toType: Class[T]) extends ExtractionError {
//    val keys = NonEmptyList.one(RootKey)
  }
  case class RequiredObjectError() extends ExtractionError {
//    val keys = NonEmptyList.one(key)
  }
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

  abstract class JsonProducer extends StringProducer with IntProducer with BoolProducer with DoubleProducer
    with ObjectProducer with ListProducer with JsonKeyResolver

  abstract class JsonConsumer

  // a function that takes a JsonProducer as input
//  type FromProducer[A] = JsonProducer => A
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

        case op: ToHListDataDefinitionOp[a] => {
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
            case _: IllegalArgumentException => Invalid(NonEmptyList.one(ConversionError(uuidString, classOf[UUID])))
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
              case NonFatal(ex) => Invalid(NonEmptyList.one(ConversionError(str, classOf[Date])))
            }
            case None => Invalid(NonEmptyList.one(RequiredObjectError()))
          }

        }
        case EitherData(a,b) => {
          ???
//          apply(a)(jsonProducer).map(Left(_))
//            .orElse(apply(b)(jsonProducer).map(Right(_)))
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
              case ex: NumberFormatException => Invalid(NonEmptyList.one(ConversionError(str, classOf[BigDecimal])))
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
      }
  }

}

object EncoderInterpreter {


  trait AToJson[T]
  case class AToJsonPure[A](a: A)
  case class AToJsonAp[A,B](f: A => B)

//  case class AToJsonEncoder() extends cats.arrow.FunctionK[DataDefinitionOp, AToJson] {
//    override def apply[A](fa: DataDefinitionOp[A]): AToJson[A] = fa match {
//      case op: ToHListDataDefinitionOp[A] => {
//        op.encodeMembers(this).apply(input)
//      }
//    }
//  }

  type Encode[A] = A => JValue

  implicit def validateAndEncodeFreeAp = new  Applicative[Encode] {
    def pure[A](x: A): Encode[A] = {
      a => JNothing
    }

    def ap[A, B](ff: Encode[A => B])(fa: Encode[A]): Encode[B] = {
//      (b: B) => {
//        def aToB: A => B = a => {
//          (fa.apply(a), ff.apply(aToB)).mapN( (v1, v2) => v1 ++ v2)
//        }
//      }
      ???
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
        case op: ToHListDataDefinitionOp[A] => {
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
        case EitherData(aDefinition,bDefinition) => {
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
      }
  }

}
