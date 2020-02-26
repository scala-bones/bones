package com.bones.data

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.UUID

import com.bones.syntax.NoAlgebra
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{CNil, Coproduct, HList, Nat}

/** A String key and it's value description where A is the type the value.
  *
  * @param key
  * @param op
  * @tparam A
  * @tparam ALG Coprodcut Value
  */
case class KeyValueDefinition[ALG[_], A](
  key: String,
  op: KeyValueDefinition.CoproductDataDefinition[ALG, A],
  description: Option[String],
  example: Option[A]
)

object KeyValueDefinition {
  type CoproductDataDefinition[ALG[_], A] = Either[KvpValue[A], ALG[A]]
}

/** Useful DSL builder */
trait KeyValueDefinitionSugar {

  def kvp[ALG[_], A](key: String, valueDefinitionOp: KvpValue[A]) =
    KeyValueDefinition[ALG, A](key, Left(valueDefinitionOp), None, None)

  def kvpCov[ALG[_], A](key: String, valueDefinitionOp: ALG[A]) =
    KeyValueDefinition[ALG, A](key, Right(valueDefinitionOp), None, None)

  def kvpHList[ALG[_], H <: HList: Manifest, HL <: Nat](
    key: String,
    kvpHList: KvpHList[ALG, H, HL]) =
    KeyValueDefinition[ALG, H](key, Left(KvpHListValue(kvpHList, List.empty)), None, None)

  def kvpCoproduct[ALG[_], C <: Coproduct: Manifest](
    key: String,
    kvpCoproduct: KvpCoproduct[ALG, C]) =
    KeyValueDefinition[ALG, C](key, Left(KvpCoproductValue(kvpCoproduct)), None, None)

}

/** Starting point for obtaining a value definition. */
trait Sugar {

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(validationOp: ValidationOp[String]*) =
    StringData(validationOp.toList)

  /** Alias for string without validations. */
  val string: StringData = string()

  /* Indicates that the data tied to this value is a Float */
  def float(f: ValidationOp[Float]*) = FloatData(f.toList)

  /** Alias for float without validations. */
  val float: FloatData = float()

  /** Indicates that the data tied to this value is a short */
  def short(f: ValidationOp[Short]*) = ShortData(f.toList)

  /** Alias for short without validations */
  val short: ShortData = short()

  /** Indicates that the data tied to this value is a double */
  def double(f: ValidationOp[Double]*) = DoubleData(f.toList)

  /** Alias for double without validations */
  val double: DoubleData = double()

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
  def list[ALG[_], T: Manifest](dataDefinitionOp: KvpValue[T], v: ValidationOp[List[T]]*) =
    ListData[ALG, T](Left(dataDefinitionOp), v.toList)

  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean]*) = BooleanData(f.toList)

  val boolean: BooleanData = boolean()

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID]*) = UuidData(v.toList)

  /** Alias for UUID without validations */
  val uuid: UuidData = UuidData(List.empty)

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def localDateTime(v: ValidationOp[LocalDateTime]*) = LocalDateTimeData(v.toList)

  val localDateTime = LocalDateTimeData(List.empty)

  def localDate(v: ValidationOp[LocalDate]*) = LocalDateData(v.toList)

  val localDate: LocalDateData = LocalDateData(List.empty)

  def localTime(v: ValidationOp[LocalTime]*): LocalTimeData = LocalTimeData(v.toList)

  val localTime: LocalTimeData = localTime()

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal]*) = BigDecimalData(v.toList)

  /** Alias for bigDecimal without validations */
  val bigDecimal: BigDecimalData = bigDecimal()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](definitionA: KvpValue[A], definitionB: KvpValue[B]) =
    EitherData(Left(definitionA), Left(definitionB))

  /** Expecting the type to be a Scala style enumeration
    *
    * @param e The base enumeration type.
    * @tparam E The enumeration
    */
  def enumeration[E <: Enumeration, V: Manifest](e: E, validationOp: ValidationOp[V]*) =
    EnumerationData[E,V](e, validationOp.toList)

  /** Indicates that the data is a list of Key Value pairs */
  def kvpHList[H <: HList: Manifest, HL <: Nat, ALG[_]](
    kvpHList: KvpHList[ALG, H, HL],
    v: ValidationOp[H]*) =
    KvpHListValue(kvpHList, v.toList)

  def kvpNil = new KvpNil[NoAlgebra]()

  def kvpNilCov[ALG[_]] = new KvpNil[ALG]()

  def kvpCoNil = new KvpCoNil[NoAlgebra]

  def kvpCoNilCov[ALG[_]] = new KvpCoNil[ALG]()

  /** Indicates that the data tied to the value is an Array of Bytes */
  def byteArray(v: ValidationOp[Array[Byte]]*): ByteArrayData = ByteArrayData(v.toList)

  /** Alias for byte array without validations */
  val byteArray: ByteArrayData = byteArray()

}
