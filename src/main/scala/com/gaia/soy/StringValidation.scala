package com.gaia.soy

import java.util.UUID

import cats.{Apply, Id}
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._

import scala.util.Try
import scala.util.matching.Regex

object StringValidation {

  private val alphanumRegx = "^[a-zA-Z0-9]*$".r
  case class IsAlphanum() extends ValidationOp[String] {
    val isValid: String => Boolean = alphanumRegx.findFirstMatchIn(_).isDefined
  }
  case class Min(min: Int) extends ValidationOp[String] {
    val isValid: String => Boolean = _.length >= min
  }
  case class Max(max: Int) extends ValidationOp[String] {
    val isValid: String => Boolean = _.length <= max
  }
  case class MatchesRegex(r: Regex) extends ValidationOp[String] {
    val isValid: String => Boolean = r.findFirstMatchIn(_).isDefined
  }
  case class Length(length: Int) extends ValidationOp[String] {
    override def isValid: (String) => Boolean = _.length == length
  }
  case class Custom(f: String => Boolean) extends ValidationOp[String] {
    val isValid: String => Boolean = f
  }
  case class Guid() extends ValidationOp[String] {
    val isValid: String => Boolean = str => Try { UUID.fromString(str) }.toEither.fold(_ => false, _ => true)
  }
  case class Uppercase() extends ValidationOp[String] {
    val isValid: String => Boolean = str => str.toUpperCase === str
  }
  case class CreditCard() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ??? //luhn check
  }
  private val tokenRegex = "^[a-zA-Z0-9_]*$".r
  case class Token() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = tokenRegex.findFirstMatchIn(_).isDefined
  }
  case class Email() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ???
  }
  case class Hex() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ???
  }
  case class Base64() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ???
  }
  case class Hostname() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = ??? //RFC1123 hostname
  }
  case class Lowercase() extends ValidationOp[String] {
    override def isValid: (String) => Boolean = str => str.toLowerCase === str
  }

  case class Ip() //enum ip. ipv4, ipv6, cidr
  case class Uri() //


  def runValidation(key: Key, input: String, stringValidation: ValidationOp[String]): Validated[ValidationError[String], String] = {
    if (stringValidation.isValid(input)) {
      Valid(input)
    } else {
      Invalid(ValidationError(key, stringValidation, Some(input)))
    }
  }


  /**
    * This is for the syntactic sure of adding validation types.
    * @tparam NSE The 'Next StringExtraction' type if a new value is appended.
    */
  trait StringExtraction[NSE <: Extraction[F,String], F[_]] extends ExtractionAppend[String, F, NSE] {

    def length(size: Int): NSE = append(Length(size))
    def min(min: Int): NSE = append(Min(min))
    def max(max: Int): NSE = append(Max(max))
    def matchesRegex(r: Regex): NSE = append(MatchesRegex(r))
    def alphanum(): NSE = append(IsAlphanum())
    def guid(): NSE = append(Guid())
    def email(): NSE = append(Email())
    def token(): NSE = append(Token())
    def hex(): NSE = append(Hex())
    def base64(): NSE = append(Base64())
    def replace(): NSE = ??? //convert ???
    def hostname(): NSE = ???
    def normalize(): NSE = ??? //convert ???
    def lowercase(): NSE = append(Lowercase())


  }

  /** This is the default extraction from calling Key.string().  It is an optional string
    * without any validation so compiling simply returns the produced value.
    * The only data error that can occur is if the value from the producer is not a string type.
    * @param key The extraction key.
    */
  final case class OptionalStringOnly(key: Key) extends Extraction[Option,String] with StringExtraction[OptionalStringWithValidation,Option]
     { extraction =>

    type I = String

    def append(sv: ValidationOp[String]) : OptionalStringWithValidation =
      OptionalStringWithValidation(key, NonEmptyList.one(sv))

//    def convert[O](c: Conversion[Option,String,O]) = OptionalStringOnlyConversion(extraction,c)

    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, Option[String]] =
      stringProducer.produce[String](key).left.map(NonEmptyList.one)

  }

  /** When we want to convert from a String to another data type. */
//  final case class OptionalStringOnlyConversion[O](optionalStringOnly: OptionalStringOnly, c: Conversion[Option,String,O])
//    extends Extraction[Option,O]{
//    type I = String
//
//    override def extract(input: RawValueProducer): Either[ExtractionErrors, Option[O]] =
//      optionalStringOnly.extract(input).flatMap(e => e match {
//        case Some(str) => c.convert(str)
//          .map(o => Some(o))
//          .left.map(_ => NonEmptyList.one(ValidationError(optionalStringOnly.key, c, Some(str))))
//        case None => Right(None)
//      }
//    )
//
//  }

  /** This is an extraction without validation, but a value is required from the producer.
    * It will return an error if the producer returns a none for the key type as well
    * as if the value from the producer is not a string type.
    **/
  final case class RequiredStringOnly(key: Key) extends Extraction[Id,String] with StringExtraction[RequiredStringWithValidation,Id] {

    type I = String

    def optional() = OptionalStringOnly(key)

    def extract(input: RawValueProducer): Either[ExtractionErrors,String] =
      input.produce[String](key).left.map(NonEmptyList.one).flatMap(res => res match {
        case Some(e) => Right(e)
        case None=> Left(NonEmptyList.one(ValidationError(key, RequiredOp(), res)))
      })

    def append(sv: ValidationOp[String]) : RequiredStringWithValidation = RequiredStringWithValidation(key, NonEmptyList.one(sv))

  }

//  final case class RequiredStringOnlyConversion[O](requiredStringOnly: RequiredStringOnly, c: ConversionOp[String,O])
//    extends Extraction[Id,O]{
//
//    override type I = String
//    def extract(input: RawValueProducer): Either[ExtractionErrors, O] = {
//      requiredStringOnly.extract(input).flatMap(res =>
//        c.convert(res).left.map(_ => NonEmptyList.one(ValidationError(requiredStringOnly.key, c, Some(res))))
//      )
//    }
//
//  }

  def runAndMapValidations(key: Key, str: String, validations: NonEmptyList[ValidationOp[String]]): ValidationResultNel[String] =
    validations.map(runValidation(key, str, _))
      .foldLeft[ValidatedNel[ValidationError[String], String]](Valid(str))( (last,next) => {
        val n: ValidatedNel[ValidationError[String], String] = next.leftMap(NonEmptyList.one)
        Apply[ValidatedNel[ValidationError[String], ?]]
          .map2(last,n)( (a,b) => b)
    })


  final case class OptionalStringWithValidation(key: Key, validations: NonEmptyList[ValidationOp[String]]) extends Extraction[Option,String]
    with StringExtraction[OptionalStringWithValidation,Option] {

    def extract(input: RawValueProducer): Either[ExtractionErrors, Option[String]] = {
      input.produce[String](key).toValidatedNel.andThen(oStr => oStr match {
        case Some(str) => {
          runAndMapValidations(key, str, validations).map(Some(_))
        }
        case None => Valid(None)
      }).toEither
    }

    def append(sv: ValidationOp[String]) : OptionalStringWithValidation = OptionalStringWithValidation(key, sv :: validations)
  }

  final case class RequiredStringWithValidation(key: Key, validations: NonEmptyList[ValidationOp[String]]) extends Extraction[Id,String]
    with StringExtraction[RequiredStringWithValidation,Id]{

    def optional() : OptionalStringWithValidation = OptionalStringWithValidation(key, validations)

    def extract(input: RawValueProducer): Either[NonEmptyList[ExtractionError], String] = {
      input.produce[String](key).toValidated.leftMap(NonEmptyList.one).andThen(res => res match {
        case Some(e) => Valid(e)
        case None=> Invalid(NonEmptyList.one(ValidationError(key, RequiredOp(), res)))
      }).andThen(str =>
        runAndMapValidations(key, str, validations)
      ).toEither
    }
    def append(sv: ValidationOp[String]) : RequiredStringWithValidation = RequiredStringWithValidation(key, sv :: validations)
  }

  /******** Conversion *************/

//  trait StringConversion[O] {
//    def conversionOp: ConversionOp[String,O]
//  }
//
//  case class ToBigDecimal() extends ConversionOp[String, BigDecimal] {
//    def convert(str: String) : Either[String, BigDecimal] =
//      Try { BigDecimal(str) }.toEither.left.map(_ => "Could not convert String to BigDecimal")
//  }
//
//  case class ToUuid() extends ConversionOp[String, UUID] {
//    override def convert(iToO: String): Either[String, UUID] =
//      Try { UUID.fromString(iToO) }.toEither.left.map(_ => "Could not convert String to UUID")
//  }

}



