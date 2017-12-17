package com.gaia

import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import com.gaia.soy.Soyo.Obj
import com.gaia.soy.StringValidation.RequiredString

package object soy {

  /** Error Case */
  trait ExtractionError {
    def key: Key
  }
  case class ValidationError[T](key: Key, failedExtraction: ExtractionOp[T], input: Option[T]) extends ExtractionError
  case class WrongTypeError[T](key: Key, expectedType: Class[T], providedType: Class[_]) extends ExtractionError
  case class RequiredObjectError(key: Key) extends ExtractionError

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

  /** Represents an operation from one type to another */
//  trait ConversionOp[I,O] extends ExtractionOp {
//    def convert(iToO: I) : Either[ConversionError[I], O]
//  }
//  abstract class Conversion[F[_]:Applicative, I,O] extends Extraction[F,O] {
//
//
//  }

  /** Starting point for obtaining a value is to define a key */
  sealed abstract class Key extends Obj { thisKey =>
    def string() : RequiredString = RequiredString(thisKey, Nil)
    //    def int(): Extract[Int] = IntExtract(thisKey, IsInt())
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
    def produceInt(key: Key): Either[WrongTypeError[String], Option[Int]]
  }
  trait BoolProducer {
    def produceBoole(key: Key): Either[WrongTypeError[String], Option[Boolean]]
  }
  trait BigDecimalProducer {
    def produceBigDecimal(key: Key): Either[WrongTypeError[String], Option[BigDecimal]]
  }
  trait ObjectProducer {
    def produceObject(key: Key): Either[WrongTypeError[String], Option[JsonProducer]]
  }

  abstract class JsonProducer extends StringProducer with IntProducer with BoolProducer with BigDecimalProducer with ObjectProducer

//  case class PureExtraction[F[_]:Applicative,U](u: F[U]) extends Extraction[F, U] {
//    override type I = Nothing
//    override def extract(stringProducer: StringProducer): Either[Nothing, F[U]] = Right(u)
//  }
//
//  case class MappedExtraction[F[_]:Applicative,T,U](priorExtraction: Extraction[F,T], f: T => U) extends Extraction[F,U] {
//    override type I = priorExtraction.I
//
//    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, F[U]] =
//      priorExtraction.extract(stringProducer).right.map(ft => {
//        Functor[F].map(ft)(f)
//      })
//
//  }
//
////  case class FlatMappedExtraction[F[_]:Monad,T,U](priorExtraction: Extraction[F,T], f: T => Extraction[F,U]) extends Extraction[F,U] {
////    override type I = priorExtraction.I
////
////    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, U] = {
////      priorExtraction.extract(stringProducer).right.flatMap( r1 => {
////        Monad[F].flatMap(r1)(a => f(a).extract(stringProducer))
////      })
////    }
////
////  }
//
//  case class ApplyExtraction[F[_]:Applicative,T,U](priorExtraction: Extraction[F,T], f: Extraction[F,T => U]) extends Extraction[F,U] {
//    override type I = priorExtraction.I
//
//    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, F[U]] = {
//      val vft: Either[ExtractionErrors,F[T]] = priorExtraction.extract(stringProducer)
//      val vftu: Either[ExtractionErrors,F[T => U]] = f.extract(stringProducer)
//
//      (vft,vftu) match {
//        case (Right(ft), Right(ftu)) => Right(Apply[F].ap(ftu)(ft))
//        case (Left(e), Right(_)) => Left(e)
//        case (Right(_), Left(e)) => Left(e)
//        case (Left(e1), Left(e2)) => Left(e1 |+| e2)
//      }
//    }
//  }
//
//  /**
//    *
//    * @tparam O Output Type
//    */
//  abstract class Extraction[F[_]:Applicative,O] { self =>
//    //Input Type
//    type I
//    def extract(stringProducer: RawValueProducer) : Either[ExtractionErrors,F[O]]
//
////    def optional() = new Extraction[Option,O] {
////      self.extract()
////    }
//
//    def map[U](f: O => U): Extraction[F,U] = MappedExtraction(this, f)
////    def flatMap[U](f: O => Extraction[F,U]): Extraction[F,U] = FlatMappedExtraction(this, f)
//    def ap[U](ff: Extraction[F, O => U]): Extraction[F,U] = ApplyExtraction(this, ff)
//  }
//
//  trait RequiredExtraction[F[_]:Applicative,O] extends Extraction[F,O] {
//    def extractx(rawValueProducer: RawValueProducer) : Either[ExtractionErrors,O] = {
//      val res = extract(rawValueProducer).map(fo => fo)
//    }
//  }
//
//  object OptionalExtraction {
//    case class OptionalExtractionWithDefault[O](optionalExtraction: Extraction[Option,O], default: () => O) extends Extraction[Id,O] {
//      override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, Id[O]] =
//        optionalExtraction.extract(stringProducer).right.map(_.getOrElse(default()))
//
//      override type I = this.type
//    }
//
//  }



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

//  object Extraction {
//    case class OptionExtractionFunctor[E]() extends Functor[Extraction[Option,?]] with Monad[Extraction[Option,?]] with Apply[Extraction[Option,?]] {
//      override def flatMap[A, B](fa: Extraction[Option, A])(f: (A) => Extraction[Option, B]): Extraction[Option, B] = ???
//
//      override def tailRecM[A, B](a: A)(f: (A) => Extraction[Option, Either[A, B]]): Extraction[Option, B] = ???
//
//      override def pure[A](x: A): Extraction[Option, A] = PureExtraction[Option,A](Some(x))
//    }
//
//  }




}
