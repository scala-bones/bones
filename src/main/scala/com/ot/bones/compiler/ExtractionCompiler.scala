package com.ot.bones.compiler

import cats.Applicative
import cats.data.Validated.Valid
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.ot.bones.transform.{OptionalTransform, Transform}
import com.ot.bones.validation.CustomConversionFromString.RequiredCustomExtraction
import com.ot.bones.validation.DateValidation.OptionalDateExtraction
import com.ot.bones.validation.IntValidation.RequiredInt
import com.ot.bones.validation.StringValidation.{OptionalString, RequiredString}
import com.ot.bones.validation.ToHList._
import com.ot.bones.validation.UuidValidation.RequiredUuidExtraction
import com.ot.bones.{BonesOp, Key}
import shapeless._


object ExtractionCompiler {

  /** Error Case */
  trait ExtractionError {
    def keys: NonEmptyList[Key]
  }

  /**
    * Used to indicate a validation error.
    * @param key
    * @param failurePoint The extraction op where the error failed.
    * @param input The input, if available.
    * @tparam T the target Type
    * @tparam I the input Type (ie, the base type, for instance the base type of UUID would be String)
    */
  case class ValidationError[T,I](key: Key, failurePoint: ExtractionOp[T], input: Option[I]) extends ExtractionError {
    val keys = NonEmptyList.one(key)
  }
  case class WrongTypeError[T](key: Key, expectedType: Class[T], providedType: Class[_]) extends ExtractionError {
    val keys = NonEmptyList.one(key)
  }
  case class RequiredObjectError(key: Key) extends ExtractionError {
    val keys = NonEmptyList.one(key)
  }
  case class MultiKeyError[T](keys: NonEmptyList[Key], failurePoint: ExtractionOp[T]) extends ExtractionError


  type ExtractionErrors = NonEmptyList[ExtractionError]

  type ValidationResult[T] = Validated[ExtractionError,T]
  type ValidationResultNel[T] = Validated[NonEmptyList[ExtractionError], T]

  /**Extracting consists of Validation and Conversion.
    */
  trait ExtractionOp[T] {
    def description: String
  }

  case class RequiredOp[T]() extends ExtractionOp[T] {
    override def description: String = "required"
  }

  /** Represents a validation operation */
  trait ValidationOp[T] extends ExtractionOp[T] {
    def isValid: T => Boolean
    def defaultError(t: T): String
  }

  case class ValidValue[T](validValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: (T) => Boolean = validValues.contains
    override def defaultError(t: T): String = s"Value ${t} must be one of ${validValues.mkString("('","','","')")}"
    override def description: String = s"one of ${validValues.mkString("('","','","')")}"
  }

  case class InvalidValue[T](invalidValues: Vector[T]) extends ValidationOp[T] {
    override def isValid: (T) => Boolean = str => {! invalidValues.contains(str)}
    override def defaultError(t: T): String = s"Value ${t} must not be one of ${invalidValues.mkString("('","','","')")}"
    override def description: String = s"not one of ${invalidValues.mkString("('","','","')")}"
  }


  trait StringProducer {
    def produceString(key: Key): Either[WrongTypeError[String], Option[String]]
  }
  trait IntProducer {
    def produceInt(key: Key): Either[WrongTypeError[Int], Option[Int]]
  }
  trait BoolProducer {
    def produceBool(key: Key): Either[WrongTypeError[Boolean], Option[Boolean]]
  }
  trait BigDecimalProducer {
    def produceBigDecimal(key: Key): Either[WrongTypeError[BigDecimal], Option[BigDecimal]]
  }
  trait ObjectProducer {
    def produceObject(key: Key): Either[WrongTypeError[JsonProducer], Option[JsonProducer]]
  }

  abstract class JsonProducer extends StringProducer with IntProducer with BoolProducer with BigDecimalProducer with ObjectProducer

  /**
    * This is to we can add syntactic sugar to any Extraction types.
    * @tparam NE The Extraction type when a new Validation Op is appended.
    */
  trait ExtractionAppend[O,F[_], NE] {
    def append(sv: ValidationOp[O]) : NE

    def valid(o: O*): NE = append(ValidValue(o.toVector))
    def invalid(o: O*): NE = append(InvalidValue(o.toVector))
  }



  // a function that takes a JsonProducer as input
//  type FromProducer[A] = JsonProducer => A
  type FromProducer[A] = JsonProducer => ValidationResultNel[A]
  implicit def fromProducerApp = new Applicative[FromProducer] {
    override def pure[A](x: A): FromProducer[A] = json => Valid(x)
    override def ap[A, B](ff: FromProducer[A => B])(fa: FromProducer[A]): FromProducer[B] =
      jsonProducer => {
        val f = ff(jsonProducer)
        val a = fa(jsonProducer)
        (f,a).mapN( (fab, a) => fab.apply(a))
      }
  }

  /** Compiler responsible for extracting data from JSON */
  case class DefaultExtractCompiler() extends cats.arrow.FunctionK[BonesOp, FromProducer] {
    def apply[A](fgo: BonesOp[A]): FromProducer[A] = jsonProducer =>
      fgo match {
        case key: Key => {
          jsonProducer.produceObject(key).leftMap(NonEmptyList.one).toValidated
        }
        case op: ToHListBonesOp[a] => {
          op.extract(this)(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        }
        case op: ToOptionalHListBonesOp[a] => {
          op.extract(this)(jsonProducer).asInstanceOf[ValidationResultNel[A]]
        }
        case op: RequiredString => op.extract(jsonProducer)
        case op: OptionalString  => op.extract(jsonProducer)
        case op: RequiredInt => op.extract(jsonProducer)
        case op: OptionalDateExtraction => op.extract(jsonProducer)
        case op: OptionalTransform[a,b] => op.extract(this)(jsonProducer)
//        case op: ObjectFieldGroup[a,z] => op.extract(jsonProducer)
        case op: Transform[A,a] => op.extract(this)(jsonProducer)
        case op: RequiredUuidExtraction => op.extract(jsonProducer)
        case op: RequiredCustomExtraction[a] => op.extract(jsonProducer)
//
//        case _ => ???
      }
  }


}
