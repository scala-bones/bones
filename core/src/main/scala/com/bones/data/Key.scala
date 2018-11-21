package com.bones.data

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{HList, Nat}

/**
  * A field definition is essentially a key value pair and a list of validations to be applied to the value.
  * @tparam A The type this field definition is describing.
  */
case class KeyValueDefinition[A](key: String, op: ValueDefinitionOp[A]) {

//  def xmap[B:Manifest](fab: A => B, fba: B => A) = KeyValueDefinition(key, XMapData(op, fab, fba), List.empty)
//
//  def convert[B:Manifest](fab: A => Either[CanNotConvert[A,B], B], fba: B => Either[CanNotConvert[B,A],A], bValidations: List[ValidationOp[B]])
//    : KeyValueDefinition[B] = KeyValueDefinition(key, Convert[A,B](op, fab, fba), bValidations)

}


trait KeyValueDefinitionSugar {
  def kvp[A](key: String, valueDefinitionOp: ValueDefinitionOp[A]) = KeyValueDefinition(key, valueDefinitionOp)
  def kvpGroup[H<:HList, HL<:Nat](key: String, kvpGroup: KvpGroup[H,HL]) = KeyValueDefinition(key, KvpGroupData(kvpGroup, List.empty))
  def kvpValue[A](key: String, value: DataClass[A]) = KeyValueDefinition[A](key, KvpValueData(value, List.empty))
}


/** Starting point for obtaining a value is to define a key */
trait Sugar {


  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(validationOp: ValidationOp[String]*) = StringData(validationOp.toList)

  val string: StringData = string()

  def byteReference(v: ValidationOp[ByteReference] *) = ByteReferenceData(v.toList)

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def int(f: ValidationOp[Int] *) = IntData(f.toList)

  val int: IntData = int()

  /** Indicates that the data tied to this key is an Double type that must pass the specified validations.
    * In the JSON world, this would be a number which is converted to a Double.
    **/
  def double(f: ValidationOp[Double]*) = DoubleData(f.toList)

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
  def list[T, L <: List[T]:Manifest](dataDefinitionOp: ValueDefinitionOp[T], v: ValidationOp[L]*) =
    ListData(dataDefinitionOp, v.toList)


  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean]*) = BooleanData(f.toList)

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID] *) = UuidData(v.toList)

  val uuid: UuidData = uuid()

  /** Indicates that the data tied to this key is a Date type with the especified format that must pass the specified validations. */
  def date(dateFormat: DateTimeFormatter, formatDescription: String, v: ValidationOp[ZonedDateTime] *) =
    DateData(dateFormat, formatDescription, v.toList)

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal] *) =
    BigDecimalFromString(v.toList)

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A:Manifest,B:Manifest](definitionA: ValueDefinitionOp[A], definitionB: ValueDefinitionOp[B]) =
    EitherData(definitionA, definitionB)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime(v: ValidationOp[ZonedDateTime] *) =
    DateData(
        DateTimeFormatter.ISO_DATE_TIME,
        "ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'",
        v.toList)

  val isoDateTime: DateData = isoDateTime()


  /** Expecting a string that is in the format of an iso date */
  def isoDate(v: ValidationOp[ZonedDateTime]*) =
    DateData(
      DateTimeFormatter.ISO_LOCAL_DATE,
      "ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'",
      v.toList
    )

  def enumeration[A:Manifest](e: Enumeration): EnumerationStringData[A] = EnumerationStringData[A](e, List.empty)

//  def enumeration(e: Enumeration)(v: ValidationOp[e.DataClass] with ToOptionalValidation[e.DataClass]*): KeyValueDefinition[e.DataClass] =
//    KeyValueDefinition(thisKey, EnumerationStringData(e), v.toList)


  def enum[A <: Enum[A]: Manifest](enums: List[A], v: ValidationOp[A]*) =
    EnumStringData[A](enums, v.toList)

  def enum[A <: Enum[A]: Manifest](enums: List[A]): EnumStringData[A] =
    EnumStringData[A](enums, List.empty)


  def kvpGroup[H<:HList,HL<:Nat](kvpGroup: KvpGroup[H,HL], v: ValidationOp[H]*) = KvpGroupData(kvpGroup, v.toList)

//  def obj[A <: HList,AL <: Nat](child: KvpGroup[A, AL]) =
//    KeyValueDefinition[A](thisKey, child, List.empty)


}




