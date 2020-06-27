package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{Coproduct, HList, Nat}

/** A String key and it's value description where A is the type the value.
  *
  * @param key This is the sting token defining the Value.
  * @param dataDefinition This is the GADT representing a value type, can be either from the core Algebra or a custom Algebra.
  * @tparam A This is the type which is "wrapped" by the GADT.
  * @tparam ALG Defines what algebra(s) we can use in a context.
  *             It can be [[com.bones.data.custom.AllCustomSyntax]]  (aka everything supported in Bones).
  *             It can be a single custom algebra such as [[com.bones.data.custom.JavaTimeValue]]
  *             It can be any [[shapeless.Coproduct]] of Algebras.
  */
case class KeyValueDefinition[ALG[_], A](
  key: String,
  dataDefinition: Either[KvpCollection[ALG, A], ALG[A]],
  description: Option[String],
  example: Option[A]
)

object KeyValueDefinition {
  type CoproductDataDefinition[ALG[_], A] = Either[KvpCollection[ALG, A], ALG[A]]
}

/** Useful DSL builder */
trait KeyValueDefinitionSugar {

  def kvp[ALG[_], A](
    key: String,
    valueDefinitionOp: KvpCollection[ALG, A]): KeyValueDefinition[ALG, A] =
    KeyValueDefinition[ALG, A](key, Left(valueDefinitionOp), None, None)

  def kvpCov[ALG[_], A](key: String, valueDefinitionOp: ALG[A]): KeyValueDefinition[ALG, A] =
    KeyValueDefinition[ALG, A](key, Right(valueDefinitionOp), None, None)

  def kvpHList[ALG[_], H <: HList: Manifest, HL <: Nat](
    key: String,
    kvpHList: KvpHList[ALG, H, HL]
  ): KeyValueDefinition[ALG, H] =
    KeyValueDefinition[ALG, H](key, Left(KvpHListValue(kvpHList, List.empty)), None, None)

  def kvpCoproduct[ALG[_], C <: Coproduct: Manifest](
    key: String,
    kvpCoproduct: KvpCoproduct[ALG, C]
  ): KeyValueDefinition[ALG, C] =
    KeyValueDefinition[ALG, C](key, Left(KvpCoproductValue(kvpCoproduct)), None, None)

}

/** Starting point for obtaining a value definition. */
trait Sugar[ALG[_]] {

  implicit class WrapKvpValueInCollection[A: Manifest](hm: ALG[A]) { self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional: OptionalKvpValueDefinition[ALG,A] =
      OptionalKvpValueDefinition[ALG, A](Right(hm))
  }

  implicit class WrapKvpCollectionInCollection[A: Manifest](hm: KvpCollection[ALG, A]) { self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Left(hm), validationOps.toList)

    def optional: OptionalKvpValueDefinition[ALG, A] =
      OptionalKvpValueDefinition[ALG, A](Left(hm))
  }

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  AllCustomAlgebras values are type
    * T and all values must pass the list of validations.
    *
    * @param dataDefinitionOp - One of the supported KvpValue types.
    * @param v List of validations each element of the list must pass to be valid.
    * @tparam T The type of each element.  Can be an EitherFieldDefinition if more than one type is expected in the list.
    * @return
    */
  def list[T: Manifest](
    dataDefinitionOp: ALG[T],
    v: ValidationOp[List[T]]*
  ): ListData[ALG, T] =
    ListData[ALG, T](Right(dataDefinitionOp), v.toList)

  def list[T: Manifest](
    kvpValue: KvpCollection[ALG, T],
    v: ValidationOp[List[T]]*
  ): ListData[ALG, T] =
    ListData[ALG, T](Left(kvpValue), v.toList)

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] =
    EitherData(Right(definitionA), Right(definitionB))

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](
    definitionA: KvpCollection[ALG, A],
    definitionB: KvpCollection[ALG, B]
  ): EitherData[ALG, A, B] =
    EitherData(Left(definitionA), Left(definitionB))

  def either[A: Manifest, B: Manifest](
    definitionA: KvpCollection[ALG, A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] =
    EitherData(Left(definitionA), Right(definitionB))

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: KvpCollection[ALG, B]
  ): EitherData[ALG, A, B] =
    EitherData(Right(definitionA), Left(definitionB))

  /** Indicates that the data is a list of Key Value pairs */
  def kvpHList[H <: HList: Manifest, HL <: Nat](
    kvpHList: KvpHList[ALG, H, HL],
    v: ValidationOp[H]*
  ): KvpHListValue[ALG, H, HL] =
    KvpHListValue(kvpHList, v.toList)

  def kvpNil = new KvpNil[ALG]()

  def kvpCoNil = new KvpCoNil[ALG]()

}
