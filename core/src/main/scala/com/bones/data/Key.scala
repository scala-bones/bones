package com.bones.data

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import com.bones.data.Error.CanNotConvert
import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.ListValidation.PassesAll
import com.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.{Generic, HList, HNil, Nat}

trait KeySyntax {
  def key(key: String) = Key(key)

  /** Implicit to turn as string into a key. */
  implicit class StringToKey(str: String) {
    def key(): Key = Key(str)
  }
}

/**
  * A field definition is essentially a key value pair and a list of validations to be applied to the value.
  * @tparam A The type this field definition is describing.
  */
trait KeyValueDefinition[A] {
  /** String key, aka field name */
  val key: Key
  /** One of the Data Definitions describing the data of this field. */
  val op: ValueDefinitionOp[A]
  /** List of validations this field should adhere to*/
  val validations: List[ValidationOp[A]]

  import shapeless.::
  val asKvp: KvpGroup[A :: HNil, Nat._1] = this :: KvpNil
}

/** Indicates that the field is Optional */
case class OptionalFieldDefinition[A](key: Key, op: ValueDefinitionOp[A], validations: List[ValidationOp[A]])
  extends KeyValueDefinition[A]

/** Indicates that the field is required */
case class RequiredFieldDefinition[A](key: Key,
                                      op: ValueDefinitionOp[A]  with ToOptionalData[A],
                                      validations: List[ValidationOp[A] with ToOptionalValidation[A]])
  extends KeyValueDefinition[A] {
  def optional(): OptionalFieldDefinition[Option[A]] = {
    val optionalValidations = validations.map(_.toOption)
    OptionalFieldDefinition(key, op.toOption, optionalValidations)
  }

  def convert[B](fab: A => Either[CanNotConvert[A,B], B], fba: B => A, description: String, validations: List[ValidationOp[B] with ToOptionalValidation[B]]): ConversionFieldDefinition[A,B] = {
    val cd = ConversionData[A,B](op, fab, fba, description)
    ConversionFieldDefinition[A,B](this, cd, validations)
  }

  def transform[Z:Manifest]()(implicit gen: Generic.Aux[Z, A]): ConversionFieldDefinition[A,Z] = {
    val newOp = ConversionData(op, (a: A) => Right(gen.from(a)), gen.to, s"Transform to type ${manifest[Z].runtimeClass.getSimpleName}")
    ConversionFieldDefinition(this, newOp, List.empty)
  }

}

case class ConversionFieldDefinition[A,B](convertFrom: RequiredFieldDefinition[A], op: ConversionData[A,B], validations: List[ValidationOp[B] with ToOptionalValidation[B]])
  extends KeyValueDefinition[B] {
  /** String key, aka field name */
  override val key: Key = convertFrom.key

  def optional(): OptionalFieldDefinition[Option[B]] = {
    val optionalValidations = validations.map(_.toOption)
    OptionalFieldDefinition(key, op.toOption, optionalValidations)
  }
}


/** Starting point for obtaining a value is to define a key */
case class Key(name: String) { thisKey =>

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(f: ValidationOp[String] with ToOptionalValidation[String] *): RequiredFieldDefinition[String] = RequiredFieldDefinition(this, StringData(), f.toList)

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def int(f: ValidationOp[Int] with ToOptionalValidation[Int] *): RequiredFieldDefinition[Int] = RequiredFieldDefinition[Int](this, IntData(), f.toList)

  /** Indicates that the data tied to this key is an Double type that must pass the specified validations.
    * In the JSON world, this would be a number which is converted to a Double.
    **/
  def double(f: ValidationOp[Double] with ToOptionalValidation[Double] *): RequiredFieldDefinition[Double] =
    RequiredFieldDefinition[Double](this, DoubleData(), List.empty)

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  All values are type
    * T and all values must pass the list of validations.
    *
    * @param dataDefinitionOp - One of the supported ValueDefinitionOp types.
    * @param v List of validations each element of the list must pass to be valid.
    * @tparam T The type of each element.  Can be an EitherFieldDefinition if more than one type is expected in the list.
    * @tparam L The List[T] type.
    * @return
    */
  def list[T, L <: List[T]](dataDefinitionOp: ValueDefinitionOp[T], v: ValidationOp[T] with ToOptionalValidation[T]*): RequiredFieldDefinition[L] =
    RequiredFieldDefinition[L](this, ListData(dataDefinitionOp), List(PassesAll[T,L](v.toList)))


  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean] with ToOptionalValidation[Boolean] *): RequiredFieldDefinition[Boolean] =
    RequiredFieldDefinition[Boolean](this, BooleanData(), f.toList)

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID] with ToOptionalValidation[UUID] *): RequiredFieldDefinition[UUID] = RequiredFieldDefinition[UUID](this, UuidData(), v.toList)

  /** Indicates that the data tied to this key is a Date type with the especified format that must pass the specified validations. */
  def date(dateFormat: DateTimeFormatter, formatDescription: String, v: ValidationOp[ZonedDateTime] with ToOptionalValidation[ZonedDateTime]*): RequiredFieldDefinition[ZonedDateTime] =
    RequiredFieldDefinition(thisKey, DateData(dateFormat, formatDescription), v.toList)

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal] with ToOptionalValidation[BigDecimal]*): RequiredFieldDefinition[BigDecimal] =
    RequiredFieldDefinition[BigDecimal](thisKey, BigDecimalFromString(), v.toList)

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A,B](definitionA: ValueDefinitionOp[A], definitionB: ValueDefinitionOp[B]) : RequiredFieldDefinition[Either[A,B]] =
    RequiredFieldDefinition[Either[A,B]](thisKey, EitherData(definitionA, definitionB), List.empty)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime(v: ValidationOp[ZonedDateTime] with ToOptionalValidation[ZonedDateTime]*): RequiredFieldDefinition[ZonedDateTime] =
    RequiredFieldDefinition(thisKey,
      DateData(
        DateTimeFormatter.ISO_DATE_TIME,
        "ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'"
      ),
      v.toList
    )


  /** Expecting a string that is in the format of an iso date */
  def isoDate(v: ValidationOp[ZonedDateTime] with ToOptionalValidation[ZonedDateTime]*): RequiredFieldDefinition[ZonedDateTime] = RequiredFieldDefinition(thisKey, isoDateData, v.toList)
  private val isoDateData = DateData(
    DateTimeFormatter.ISO_LOCAL_DATE,
    "ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'"
  )

  def enumeration(e: Enumeration): RequiredFieldDefinition[e.Value] =
    RequiredFieldDefinition(thisKey, EnumerationStringData(e), List.empty)

//  def enumeration(e: Enumeration)(v: ValidationOp[e.Value] with ToOptionalValidation[e.Value]*): RequiredFieldDefinition[e.Value] =
//    RequiredFieldDefinition(thisKey, EnumerationStringData(e), v.toList)


  def enum[A <: Enum[A]: Manifest](enums: List[A], v: ValidationOp[A] with ToOptionalValidation[A]*) : RequiredFieldDefinition[A] =
    RequiredFieldDefinition[A](thisKey, EnumStringData[A](enums), v.toList)
  def enum[A <: Enum[A]: Manifest](enums: List[A]) : RequiredFieldDefinition[A] =
    RequiredFieldDefinition[A](thisKey, EnumStringData[A](enums), List.empty)

  def obj[A <: HList,AL <: Nat](child: KvpGroup[A, AL]): RequiredFieldDefinition[A] =
    RequiredFieldDefinition[A](thisKey, child, List.empty)


}

object Sugar {
  /** Aliases for creating HList types. */
  trait ToHList {

    def stringData: StringData = StringData()

    def either[A, B](op1: ValueDefinitionOp[A], op2: ValueDefinitionOp[B]): EitherData[A, B] = EitherData[A, B](op1, op2)
  }

}



