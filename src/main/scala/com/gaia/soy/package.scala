package com.gaia

import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import com.gaia.soy.IntValidation.RequiredInt
import com.gaia.soy.StringValidation.RequiredString
import com.gaia.soy.validation.{CustomConversionFromString, DateValidation, UuidValidation}

package object soy {

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

  trait Metadata
  case class Description[T](description: String) extends Metadata
  case class Notes[T](notes: List[String]) extends Metadata
  case class Meta[T](meta: String) extends Metadata
  case class Tags[T](tags: List[String]) extends Metadata
  case class Example[T](example: String) extends Metadata
  case class UnitOfMeasure[T](desc: String) extends Metadata

  /** Starting point for obtaining a value is to define a key */
  sealed abstract class Key extends ObjAlias { thisKey =>
    val key = thisKey
    def string() : RequiredString = RequiredString(thisKey, Nil)
    def int(): RequiredInt = RequiredInt(thisKey, Nil)
    //    def BigDecimal(): Extract[Int] = ???
    //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
    //      override def validation = CanBeEither[A,B](v1, v2)
    //      override val key = thisKey
    //    }
    //    def array(): Extract[Vector[Int]] = ???
    //    def boolean(): Extract[Boolean] = ???
    //    def binary(): Extract[Boolean] = ???  //maybe this is a string().binary().
    //    def date(): Extract[Date] = ???
  }

  object RootKey extends Key
  case class StringKey(name: String) extends Key

  /** FieldGroupOp is the base class defining the FreeAp for each field group defined.*/
  trait FieldGroupOp[A] {
    def extract(producer: JsonProducer): A
    def lift: FieldGroup[A] = FreeApplicative.lift(this)
  }

  type FieldGroup[A] = FreeApplicative[FieldGroupOp, A]



  /** Turn a string key into an key type */
  def key(key: String) = StringKey(key)
  implicit class StringToKey(str: String) {
    def key(): Key = StringKey(str)
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
    def appendMetadata(md: Metadata) : NE

    def valid(o: O*): NE = append(ValidValue(o.toVector))
    def invalid(o: O*): NE = append(InvalidValue(o.toVector))
    def description(description: String): NE = appendMetadata(Description(description))
    def notes(notes: List[String]): NE = appendMetadata(Notes(notes))
    def tags(tags: List[String]): NE = appendMetadata(Tags(tags))
    def meta(meta: String): NE = appendMetadata(Meta(meta))
    def example(example: String): NE = appendMetadata(Example(example))
    def unitOfMeasure(unit: String): NE = appendMetadata(UnitOfMeasure(unit))

    def nullIsNone() = ???
  }


  object conversions extends UuidValidation with CustomConversionFromString with DateValidation
  object obj extends Obj

}
