package com.ot.bones.interpreter

import cats.Applicative
import cats.arrow.Compose
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.BooleanDataDefinition.{OptionalBoolean, RequiredBoolean}
import com.ot.bones.IntDataDefinition.{OptionalInt, RequiredInt}
import com.ot.bones.StringDataDefinition.{OptionalString, RequiredString}
import com.ot.bones.ToHList.{ToHListDataDefinitionOp, ToOptionalHListDataDefinitionOp}
import com.ot.bones.convert.BigDecimalValidation.ConvertBigDecimal
import com.ot.bones.convert.CustomConversionFromString.CustomConversion
import com.ot.bones.convert.CustomConversionFromStringInstances
import com.ot.bones.convert.string.StringConversionOp
import com.ot.bones.interpreter.ExtractionInterpreter.ValidationResultNel
import com.ot.bones.transform.{OptionalTransform, Transform}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import com.ot.bones.validation.{DataDefinitionOp, Key}
import net.liftweb.json.JsonAST._
import shapeless.HList


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
    def resolve(key: Key): JsonProducer
  }

  abstract class JsonProducer extends StringProducer with IntProducer with BoolProducer with DoubleProducer
    with ObjectProducer with ListProducer with JsonKeyResolver

  abstract class JsonConsumer

  type ConvertString[A] = String => Validated[NonEmptyList[ExtractionError], A]
  object StringConverterInterpreter extends cats.arrow.FunctionK[StringConversionOp, ConvertString] {
    override def apply[A](fa: StringConversionOp[A]): ConvertString[A] = str => fa match {
      case cbd: ConvertBigDecimal => cbd.convertFromStringAndValidate(str).asInstanceOf[Validated[NonEmptyList[ExtractionError], A]]
      case _ => ???
    }
  }

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
        case op: ToHListDataDefinitionOp[a] => {
          op.extract(this).apply(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        }
        case op: ToOptionalHListDataDefinitionOp[a] => {
          op.extractRoot(this)(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        }
        case op: RequiredString => op.extract(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        case op: OptionalString  => op.extract(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        case op: RequiredInt => op.extract(jsonProducer).asInstanceOf[ValidationResultNel[A]]
//        case op: OptionalTransform[a,b] => op.extract(this)(jsonProducer)
//        case op: ObjectFieldGroup[a,z] => op.extract(jsonProducer)
//        case op: Transform[A,a] => op.extract(this)(jsonProducer)
//
//        case _ => ???
      }
  }

}

object DataDefinitionOpToJValue {

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition..*/
  trait JValueDataDefinitionOp[J] {
    //lift any JValueDataDefinition into a FreeApplicative
    def lift: JValueDataDefinition[J] = FreeApplicative.lift(this)
  }

  type JValueDataDefinition[J] = FreeApplicative[JValueDataDefinitionOp, J]

  case class JValueDataDefinitionWrap[A,J](dataDefinitionOp: DataDefinitionOp[A]) extends JValueDataDefinitionOp[J]

  implicit class ToHListDataDefinitionOpToJValue[L <: HList](hListDataDefinitionOp: ToHListDataDefinitionOp[L]) {
    def toJValue[JObject]: JValueDataDefinitionWrap[L,JObject] =
      JValueDataDefinitionWrap[L, JObject](hListDataDefinitionOp)
  }

}

object EncoderInterpreter {


  trait AToJson[T]
  case class AToJsonPure[A](a: A)
  case class AToJsonAp[A,B](f: A => B)

//  case class AToJsonEnconcer() extends cats.arrow.FunctionK[DataDefinitionOp, AToJson] {
//    override def apply[A](fa: DataDefinitionOp[A]): AToJson[A] = fa match {
//      case op: ToHListDataDefinitionOp[A] => {
//        op.encodeMembers(this).apply(input)
//      }
//    }
//  }

  type ValidateAndEncode[A] = A => ValidationResultNel[JValue]

  implicit def validateAndEncodeFreeAp = new  Applicative[ValidateAndEncode] {
    def pure[A](x: A): ValidateAndEncode[A] = {
      a => Valid(JNothing)
    }

    def ap[A, B](ff: ValidateAndEncode[A => B])(fa: ValidateAndEncode[A]): ValidateAndEncode[B] = {
//      (b: B) => {
//        def aToB: A => B = a => {
//          (fa.apply(a), ff.apply(aToB)).mapN( (v1, v2) => v1 ++ v2)
//        }
//      }
      ???
    }
  }


  object DefaultEncoderInterpreter {

  }

  case class DefaultEncoderInterpreter() extends cats.arrow.FunctionK[DataDefinitionOp, ValidateAndEncode] {

    def apply[A](fgo: DataDefinitionOp[A]): ValidateAndEncode[A] = (input: A) =>
      fgo match {
        case op: ToHListDataDefinitionOp[A] => {
          op.encodeMembers(this).apply(input)
        }
        case ob: OptionalBoolean => input.asInstanceOf[Option[Boolean]] match {
          case Some(b) => Valid(JBool(b))
          case None => Valid(JNull)
        }
        case rb: RequiredBoolean => Valid(JBool(input.asInstanceOf[Boolean]))
        case rs: RequiredString => Valid(JString(input.asInstanceOf[String]))
        case os: OptionalString => input.asInstanceOf[Option[String]] match {
          case Some(str) => Valid(JString(str))
          case None => Valid(JNothing)
        }
        case ri: RequiredInt => Valid(JInt(input.asInstanceOf[Int]))
        case oi: OptionalInt => input.asInstanceOf[Option[Int]] match {
          case Some(i) => Valid(JInt(i))
          case None => Valid(JNull)
        }
      }

  }

}
