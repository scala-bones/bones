package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp

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
case class KeyDefinition[ALG[_], A](
  key: String,
  dataDefinition: Either[HigherOrderValue[ALG, A], ALG[A]],
  typeName: String,
  description: Option[String],
  example: Option[A]
)

object KeyDefinition {

  /** In the context of a given ALG, the data definition can be a collection type (left) or a value type (right) */
  type CoproductDataDefinition[ALG[_], A] = Either[HigherOrderValue[ALG, A], ALG[A]]
}

/** Useful DSL builder */
trait KeyValueDefinitionSugar {

  def kvp[ALG[_], A: Manifest](
    key: String,
    valueDefinitionOp: HigherOrderValue[ALG, A]): KeyDefinition[ALG, A] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    KeyDefinition[ALG, A](key, Left(valueDefinitionOp), typeName, None, None)
  }

  def kvpCov[ALG[_], A: Manifest](key: String, valueDefinitionOp: ALG[A]): KeyDefinition[ALG, A] = {
    val typeName = manifest[A].runtimeClass.getSimpleName
    KeyDefinition[ALG, A](key, Right(valueDefinitionOp), typeName, None, None)
  }

}

/** Starting point for obtaining a value definition. */
trait Sugar[ALG[_]] {

  implicit class WrapKvpValueInCollection[A: Manifest](hm: ALG[A]) { self =>
    val typeName = manifest[A].runtimeClass.getSimpleName

    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Right(hm), typeName, validationOps.toList)

    def optional: OptionalValue[ALG, A] =
      OptionalValue[ALG, A](Right(hm), typeName)
  }

  implicit class WrapKvpCollectionInCollection[A: Manifest](hm: HigherOrderValue[ALG, A]) {
    self =>
    val typeName = manifest[A].runtimeClass.getSimpleName
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Left(hm), typeName, validationOps.toList)

    def optional: OptionalValue[ALG, A] =
      OptionalValue[ALG, A](Left(hm), typeName)
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
  ): ListData[ALG, T] = {
    val typeName = manifest[T].runtimeClass.getSimpleName
    ListData[ALG, T](Right(dataDefinitionOp), typeName, v.toList)
  }

  def list[T: Manifest](
    kvpValue: HigherOrderValue[ALG, T],
    v: ValidationOp[List[T]]*
  ): ListData[ALG, T] = {
    val typeName = manifest[T].runtimeClass.getSimpleName
    ListData[ALG, T](Left(kvpValue), typeName, v.toList)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: (String, ALG[A]),
    definitionB: (String, ALG[B])
  ): EitherData[ALG, A, B] =
    EitherData(Right(definitionA._2), definitionA._1, Right(definitionB._2), definitionB._1)

  def either[A: Manifest, B: Manifest](
    definitionA: (String, ALG[A]),
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] = {
    val typeNameOfB = manifest[B].runtimeClass.getSimpleName
    EitherData(Right(definitionA._2), definitionA._1, Right(definitionB), typeNameOfB)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: (String, ALG[B])
  ): EitherData[ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getSimpleName
    EitherData(Right(definitionA), typeNameOfA, Right(definitionB._2), definitionB._1)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getSimpleName
    val typeNameOfB = manifest[B].runtimeClass.getSimpleName
    EitherData(Right(definitionA), typeNameOfA, Right(definitionB), typeNameOfB)
  }

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](
    definitionA: HigherOrderValue[ALG, A],
    definitionB: HigherOrderValue[ALG, B]
  ): EitherData[ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getSimpleName
    val typeNameOfB = manifest[B].runtimeClass.getSimpleName
    EitherData(Left(definitionA), typeNameOfA, Left(definitionB), typeNameOfB)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: HigherOrderValue[ALG, A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getSimpleName
    val typeNameOfB = manifest[B].runtimeClass.getSimpleName
    EitherData(Left(definitionA), typeNameOfA, Right(definitionB), typeNameOfB)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: HigherOrderValue[ALG, B]
  ): EitherData[ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getSimpleName
    val typeNameOfB = manifest[B].runtimeClass.getSimpleName
    EitherData(Right(definitionA), typeNameOfA, Left(definitionB), typeNameOfB)
  }

  def kvpNil = new KvpNil[ALG]()

  def kvpCoNil = new KvpCoNil[ALG]()

}
