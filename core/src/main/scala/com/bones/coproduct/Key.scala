package com.bones.coproduct

import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{CNil, Coproduct, HList, Nat}

/** A String key and it's value description where A is the type the value.
  *
  * @param key
  * @param op
  * @tparam A
  * @tparam COV Coprodcut Value
  */
case class KeyValueDefinition[A,COV[_]](key: String, op: KeyValueDefinition.CoproductDataDefinition[A,COV])

object KeyValueDefinition {
  type CoproductDataDefinition[A, COV[_]] = Either[KvpValue[A], COV[A]]
  type NoAlgebra[A] = CNil

}

/** Useful DSL builder */
trait KeyValueDefinitionSugar {

  def kvp[A, COV[_]](key: String, valueDefinitionOp: KvpValue[A]) =
    KeyValueDefinition[A,COV](key, Left(valueDefinitionOp))

  def kvpCov[A, COV[_]](key: String, valueDefinitionOp: COV[A]) =
    KeyValueDefinition[A,COV](key, Right(valueDefinitionOp))

  def kvpHList[H <: HList: Manifest, HL <: Nat, COV[_]](key: String,
                                                kvpHList: KvpHList[H, HL, COV]) =
    KeyValueDefinition[H,COV](key, Left(KvpHListValue(kvpHList, List.empty)))

  def kvpCoproduct[C<:Coproduct:Manifest, COV[_]](key: String,
                                 kvpCoproduct: KvpCoproduct[C, COV]) =
    KeyValueDefinition[C,COV](key, Left(KvpCoproductValue(kvpCoproduct)))

}



/** Starting point for obtaining a value definition. */
trait Sugar {

  import KeyValueDefinition._

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
  def list[T: Manifest, COV[_]](dataDefinitionOp: KvpValue[T],
                        v: ValidationOp[List[T]]*) =
    ListData[T,COV](Left(dataDefinitionOp), v.toList)

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

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal]*) = BigDecimalData(v.toList)

  /** Alias for bigDecimal without validations */
  val bigDecimal: BigDecimalData = bigDecimal()

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](definitionA: KvpValue[A],
                                       definitionB: KvpValue[B]) =
    EitherData(Left(definitionA), Left(definitionB))

  /** Expecting the type to be a Scala style enumeration
    *
    * @param e The base enumeration type.
    * @tparam E The enumeration
    */
  def enumeration[E<:Enumeration,V:Manifest](e: E): EnumerationData[E,V] = {
    EnumerationData[E,V](e,List.empty)
  }


  /** Indicates that the data is a list of Key Value pairs */
  def kvpHList[H <: HList: Manifest, HL <: Nat, COV[_]](kvpHList: KvpHList[H, HL, COV],
                                                v: ValidationOp[H]*) =
    KvpHListValue(kvpHList, v.toList)

  def kvpNil = new KvpNil[NoAlgebra]()

  def kvpNilCov[COV[_]] = new KvpNil[COV]()

  def kvpCoNil = new KvpCoNil[NoAlgebra]

  def kvpCoNilCov[COV[_]] = new KvpCoNil[COV]()

  /** Indicates that the data tied to the value is an Array of Bytes */
  def byteArray(v: ValidationOp[Array[Byte]]*): ByteArrayData = ByteArrayData(v.toList)

  /** Alias for byte array without validations */
  val byteArray: ByteArrayData = byteArray()

//  def sumType[A:Manifest](subclassSchemas: List[HListConvert[_,_,A]], typeToConversion: A => HListConvert[_,_,A]): SumTypeData[A] = SumTypeData[A](subclassSchemas, typeToConversion)

//  def sumType[A](subclassSchemas: HListConvert[_,_,A]*)(validationOp: ValidationOp[A]*) = SumType(subclassSchemas.toList, validationOp.toList)

}
