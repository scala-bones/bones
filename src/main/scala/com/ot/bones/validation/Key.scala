package com.ot.bones.validation

import java.text.DateFormat
import java.time.format.DateTimeFormatter

import com.ot.bones.interpreter.ExtractionInterpreter.JsonProducer
import com.ot.bones.validation.BigDecimalValidation.RequiredBigDecimal
import com.ot.bones.validation.BooleanValidation.RequiredBoolean
import com.ot.bones.validation.DateValidation.RequiredDate
import com.ot.bones.validation.IntValidation.RequiredInt
import com.ot.bones.validation.ListValidation.RequiredList
import com.ot.bones.validation.StringValidation.RequiredString

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = StringKey(key)
  implicit class StringToKey(str: String) {
    def key(): Key = StringKey(str)
  }
}

/** Starting point for obtaining a value is to define a key */
sealed abstract class Key extends DataDefinitionOp[Option[JsonProducer]] with ObjAlias { thisKey =>
  val key = thisKey
  def string() : RequiredString = RequiredString(thisKey, List.empty)
  /** Use this if you expect the int to come in as a JSON number, otherwise use string().int() */
  def int(): RequiredInt = RequiredInt(thisKey, List.empty)
  /** Use this if you expect the bigDecimal to come in as a JSON number, otherwise use string().bigDecimal */
  def bigDecimal(): RequiredBigDecimal =  RequiredBigDecimal(Left(thisKey), List.empty)
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //def vector(): Extract[Vector[Int]] = ???
  def list[T](dataDefinitionOp: DataDefinitionOp[T]): RequiredList[T] = RequiredList(this, dataDefinitionOp)
  def boolean(): RequiredBoolean = RequiredBoolean(key)
  //    def binary(): Extract[Boolean] = ???  //maybe this is a string().binary().

  /** Date, BYOFormat */
  def date(dateFormat: DateFormat, formatDescription: Option[String] = None): RequiredDate =
    RequiredDate(key, dateFormat, formatDescription, List.empty)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime =
    RequiredDate(key,
      DateTimeFormatter.ISO_DATE_TIME.toFormat,
      Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'"),
      List.empty
    )

  /** Expecting a string that is in the format of an iso date */
  def isoDate =
    RequiredDate(key,
      DateTimeFormatter.ISO_DATE.toFormat,
      Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"),
      List.empty
    )


}

object RootKey extends Key
case class StringKey(name: String) extends Key

