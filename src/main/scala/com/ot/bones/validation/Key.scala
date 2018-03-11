package com.ot.bones.validation

import java.text.{DateFormat, SimpleDateFormat}
import java.util.{Date, UUID}

import com.ot.bones.BooleanDataDefinition.BooleanData
import com.ot.bones.DateConversionInstances.{DateConversion, DateData}
import com.ot.bones.IntDataDefinition.IntData
import com.ot.bones.StringDataDefinition.StringData
import com.ot.bones.ToHList.HList2
import com.ot.bones.UuidConversionInstances.UuidData
import com.ot.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.{::, HNil}

trait KeySyntax {
  /** Turn a string key into an key type */
  def key(key: String) = Key(key)
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

trait FieldDefinition[A] {
  val key: Key
  val op: DataDefinitionOp[A]
  val validations: List[ValidationOp[A]]
}

case class OptionalFieldDefinition[A](key: Key, op: DataDefinitionOp[A], validations: List[ValidationOp[A]])
  extends FieldDefinition[A]

case class RequiredFieldDefinition[A](key: Key, op: DataDefinitionOp[A]  with ToOptionalData[A], validations: List[ValidationOp[A] with ToOptionalValidation[A]])
  extends FieldDefinition[A] {
  def optional() = {
    val optionalValidations = validations.map(_.toOption)
    OptionalFieldDefinition(key, op.toOption, optionalValidations)
  }
}

/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>
  val key: Key = thisKey
  def string() : FieldDefinition[String] = RequiredFieldDefinition(this, StringData(), List.empty)
  def string(f: ValidationOp[String] with ToOptionalValidation[String] *): FieldDefinition[String] = RequiredFieldDefinition(this, StringData(), f.toList)
  /** Use this if you expect the int to come in as a JSON number, otherwise use string().int() */
  def int(): FieldDefinition[Int] = RequiredFieldDefinition[Int](this, IntData(), List.empty)
  def int(f: ValidationOp[Int] with ToOptionalValidation[Int] *): FieldDefinition[Int] = RequiredFieldDefinition[Int](this, IntData(), f.toList)
  /** Use this if you expect the bigDecimal to come in as a JSON number, otherwise use string().bigDecimal */
  //    def either[A,B](v1: ValidationOp[A], v2: ValidationOp[B]): Extract[Either[A,B]] = new Extract[Either[A,B]]{
  //      override def validation = CanBeEither[A,B](v1, v2)
  //      override val key = thisKey
  //    }
  //def vector(): Extract[Vector[Int]] = ???
//  def list[T](dataDefinitionOp: DataDefinitionOp[T]): RequiredList[T] = RequiredList(this, dataDefinitionOp)
  def boolean(): FieldDefinition[Boolean] = RequiredFieldDefinition[Boolean](this, BooleanData(), List.empty)
  def boolean(f: ValidationOp[Boolean] with ToOptionalValidation[Boolean] *): FieldDefinition[Boolean] =
    RequiredFieldDefinition[Boolean](this, BooleanData(), f.toList)
  def uuid(): RequiredFieldDefinition[UUID] = RequiredFieldDefinition(this, UuidData(), List.empty)
  /** Date, BYOFormat */
  def date(dateFormat: DateFormat, formatDescription: Option[String] = None): FieldDefinition[Date] =
    RequiredFieldDefinition(key, DateData(dateFormat, formatDescription), List.empty)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime(): RequiredFieldDefinition[Date] =
    RequiredFieldDefinition(key,
      DateData(
        new SimpleDateFormat("yyyy-MM-dd'T'HH:mm'Z'"),
        Some("ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'")
      ),
      List.empty
    )

  /** Expecting a string that is in the format of an iso date */
  def isoDate() =
    DateConversion(
      new SimpleDateFormat("yyyy-MM-dd"),
      Some("ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"),
      List.empty
    )


  def obj2[A, B](op1: FieldDefinition[A], op2: FieldDefinition[B]) =
    RequiredFieldDefinition[A :: B :: HNil](this, HList2(op1, op2), List.empty)
}



