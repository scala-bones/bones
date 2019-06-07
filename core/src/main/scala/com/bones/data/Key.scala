package com.bones.data

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{HList, Nat}

/** A String key and it's value description where A is the type the value. */
case class KeyValueDefinition[A](key: String, op: KvpValue[A])

/** Useful DSL builder */
trait KeyValueDefinitionSugar {
  def kvp[A](key: String, valueDefinitionOp: KvpValue[A]) =
    KeyValueDefinition(key, valueDefinitionOp)

  def kvpHList[H <: HList: Manifest, HL <: Nat](key: String,
                                                kvpHList: KvpHList[H, HL]) =
    KeyValueDefinition(key, KvpHListValue(kvpHList, List.empty))

}

/** Starting point for obtaining a value definition. */
trait Sugar {

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(validationOp: ValidationOp[String]*) =
    StringData(validationOp.toList)

  /** Alias for string without validations. */
  val string: StringData = string()

  /** Indicates the data tied to this Value is an Int */
  def int(f: ValidationOp[Int]*) = IntData(f.toList)

  /** Alias for int without any validations */
  val int: IntData = int()

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def long(f: ValidationOp[Long]*) = LongData(f.toList)

  /** Alias for long without validations. */
  val long: LongData = long()

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  All values are type
    * T and all values must pass the list of validations.
    *
    * @param dataDefinitionOp - One of the supported KvpValue types.
    * @param v List of validations each element of the list must pass to be valid.
    * @tparam T The type of each element.  Can be an EitherFieldDefinition if more than one type is expected in the list.
    * @return
    */
  def list[T: Manifest](dataDefinitionOp: KvpValue[T],
                        v: ValidationOp[List[T]]*) =
    ListData(dataDefinitionOp, v.toList)

  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean]*) = BooleanData(f.toList)

  val boolean: BooleanData = boolean()

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID]*) = UuidData(v.toList)

  /** Alias for UUID without validations */
  val uuid: UuidData = uuid()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def date(dateFormat: DateTimeFormatter,
           formatDescription: String,
           v: ValidationOp[ZonedDateTime]*) =
    DateTimeData(dateFormat, formatDescription, v.toList)

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal]*) = BigDecimalData(v.toList)

  /** Alias for bigDecimal without validations */
  val bigDecimal: BigDecimalData = bigDecimal()

  /** */
//  def double: Double = DoubleData()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](definitionA: KvpValue[A],
                                       definitionB: KvpValue[B]) =
    EitherData(definitionA, definitionB)

  /** Expecting a string that is in the format of an iso date time */
  def isoDateTime(v: ValidationOp[ZonedDateTime]*) =
    DateTimeData(
      DateTimeFormatter.ISO_DATE_TIME,
      "ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]'",
      v.toList
    )

  /** Alias for isDateTime without validations */
  val isoDateTime: DateTimeData = isoDateTime()

  /** Expecting a string that is in the format of an iso date */
  def isoDate(v: ValidationOp[ZonedDateTime]*) =
    DateTimeData(
      DateTimeFormatter.ISO_LOCAL_DATE,
      "ISO date format with the offset if available, such as '2011-12-03' or '2011-12-03+01:00'",
      v.toList
    )
  val isoDate: DateTimeData = isoDate()

  /** Expecting the type to be a Scala style enumeration
    *
    * @param e The base enumeration type.
    * @tparam A The "Value" from the enumeration, eg: EnumerationObject.Value
    */
  def enumeration[A: Manifest](e: Enumeration): EnumerationStringData[A] =
    EnumerationStringData[A](e, List.empty)

  def kvpHList[H <: HList: Manifest, HL <: Nat](kvpHList: KvpHList[H, HL],
                                                v: ValidationOp[H]*) =
    KvpHListValue(kvpHList, v.toList)

  val kvpNil = KvpNil

}
