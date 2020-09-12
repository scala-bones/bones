package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.{HList, Nat}

/** A String key and it's value description where A is the type the value.
  *
  * @param key This is the sting token defining the Value.
  * @param dataDefinition This is the GADT representing a value type.
  * @tparam A   This is the type which is "wrapped" by the GADT.
  * @tparam ALG Defines what gadt(s) we can use in a context.
  *             It can be [[com.bones.data.values.DefaultValuesSyntax]]  (aka everything supported in Bones).
  *             It can be a single data type such as [[com.bones.data.values.JavaTimeValue]]
  *             It can be any shapeless.Coproduct of Algebras.
  */
case class KeyDefinition[ALG[_], A: Manifest](
  key: String,
  dataDefinition: Either[HigherOrderValue[ALG, A], ALG[A]],
  description: Option[String],
  example: Option[A]
) {
  val manifestOfA: Manifest[A] = manifest[A]
}

object KeyDefinition {

  /** In the context of a given ALG, the data definition can be a collection type (left) or a value type (right) */
  type CoproductDataDefinition[ALG[_], A] = Either[HigherOrderValue[ALG, A], ALG[A]]
}

/** Useful DSL builder */
trait KeyValueDefinitionSugar {

  def kvp[ALG[_], A: Manifest](
    key: String,
    valueDefinitionOp: HigherOrderValue[ALG, A]): KeyDefinition[ALG, A] =
    KeyDefinition[ALG, A](key, Left(valueDefinitionOp), None, None)

  def kvpCov[ALG[_], A: Manifest](key: String, valueDefinitionOp: ALG[A]): KeyDefinition[ALG, A] =
    KeyDefinition[ALG, A](key, Right(valueDefinitionOp), None, None)

//  def kvpHList[ALG[_], H <: HList: Manifest, HL <: Nat](
//    key: String,
//    kvpHList: KvpHListCollection[ALG, H, HL]
//  ): KeyDefinition[ALG, H] =
//    KeyDefinition[ALG, H](key, Left(KvpCollectionValue(kvpHList, List.empty)), None, None)

}

/** Starting point for obtaining a value definition. */
trait Sugar[ALG[_]] {

  implicit class WrapKvpValueInCollection[A: Manifest](hm: ALG[A]) { self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional: OptionalValue[ALG, A] =
      OptionalValue[ALG, A](Right(hm))
  }

  implicit class WrapKvpCollectionInCollection[A: Manifest](hm: HigherOrderValue[ALG, A]) {
    self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Left(hm), validationOps.toList)

    def optional: OptionalValue[ALG, A] =
      OptionalValue[ALG, A](Left(hm))
  }

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  [[com.bones.data.values.DefaultValues]] values are type
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
    kvpValue: HigherOrderValue[ALG, T],
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
    definitionA: HigherOrderValue[ALG, A],
    definitionB: HigherOrderValue[ALG, B]
  ): EitherData[ALG, A, B] =
    EitherData(Left(definitionA), Left(definitionB))

  def either[A: Manifest, B: Manifest](
    definitionA: HigherOrderValue[ALG, A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] =
    EitherData(Left(definitionA), Right(definitionB))

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: HigherOrderValue[ALG, B]
  ): EitherData[ALG, A, B] =
    EitherData(Right(definitionA), Left(definitionB))

  def kvpNil = new KvpNil[ALG]()

  def kvpCoNil = new KvpCoNil[ALG]()

}
