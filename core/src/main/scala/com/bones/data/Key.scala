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
case class KeyDefinition[K, ALG[_], A](
  key: K,
  dataDefinition: Either[HigherOrderValue[K, ALG, A], ALG[A]],
  typeName: String,
  description: Option[String],
  example: Option[A]
) {

  /** Copy this KeyDefinition, using the new key based on the */
  def keyMap[KK](f: K => KK): KeyDefinition[KK, ALG, A] = {
    val newDataDef = dataDefinition match {
      case Left(hov)  => Left(hov.keyMap(f))
      case Right(alg) => Right(alg)
    }
    this.copy(key = f(key), dataDefinition = newDataDef)
  }

  def algMap[ALG2[_]](f: ALG[_] => ALG2[_]): KeyDefinition[K, ALG2, A] =
    this.copy(dataDefinition = HigherOrderValue.algMapEither[K, ALG, A, ALG2](dataDefinition, f))

}

object KeyDefinition {

  /** In the context of a given ALG, the data definition can be a collection type (left) or a value type (right) */
  type CoproductDataDefinition[K, ALG[_], A] = Either[HigherOrderValue[K, ALG, A], ALG[A]]
}

/** Useful DSL builder */
trait KeyValueDefinitionSugar {

  def kvp[K, ALG[_], A: Manifest](
    key: K,
    valueDefinitionOp: HigherOrderValue[K, ALG, A]): KeyDefinition[K, ALG, A] = {
    val typeName = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    KeyDefinition[K, ALG, A](key, Left(valueDefinitionOp), typeName, None, None)
  }

  def kvpCov[K, ALG[_], A: Manifest](
    key: K,
    valueDefinitionOp: ALG[A]): KeyDefinition[K, ALG, A] = {
    val typeName = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    KeyDefinition[K, ALG, A](key, Right(valueDefinitionOp), typeName, None, None)
  }

}

/** Starting point for obtaining a value definition. */
trait Sugar[K, ALG[_]] {

  implicit class WrapKvpValueInCollection[A: Manifest](hm: ALG[A]) { self =>
    val typeName = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")

    def list(validationOps: ValidationOp[List[A]]*): ListData[K, ALG, A] =
      ListData[K, ALG, A](Right(hm), typeName, validationOps.toList)

    def optional: OptionalValue[K, ALG, A] =
      OptionalValue[K, ALG, A](Right(hm), typeName)
  }

  implicit class WrapKvpCollectionInCollection[A: Manifest](hm: HigherOrderValue[K, ALG, A]) {
    self =>
    val typeName = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    def list(validationOps: ValidationOp[List[A]]*): ListData[K, ALG, A] =
      ListData[K, ALG, A](Left(hm), typeName, validationOps.toList)

    def optional: OptionalValue[K, ALG, A] =
      OptionalValue[K, ALG, A](Left(hm), typeName)
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
  ): ListData[K, ALG, T] = {
    val typeName = manifest[T].runtimeClass.getName.split('$').lastOption.getOrElse("")
    ListData[K, ALG, T](Right(dataDefinitionOp), typeName, v.toList)
  }

  def list[T: Manifest](
    kvpValue: HigherOrderValue[K, ALG, T],
    v: ValidationOp[List[T]]*
  ): ListData[K, ALG, T] = {
    val typeName = manifest[T].runtimeClass.getName.split('$').lastOption.getOrElse("")
    ListData[K, ALG, T](Left(kvpValue), typeName, v.toList)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: (String, ALG[A]),
    definitionB: (String, ALG[B])
  ): EitherData[K, ALG, A, B] =
    EitherData(Right(definitionA._2), definitionA._1, Right(definitionB._2), definitionB._1)

  def either[A: Manifest, B: Manifest](
    definitionA: (String, ALG[A]),
    definitionB: ALG[B]
  ): EitherData[K, ALG, A, B] = {
    val typeNameOfB = manifest[B].runtimeClass.getName.split('$').lastOption.getOrElse("")
    EitherData(Right(definitionA._2), definitionA._1, Right(definitionB), typeNameOfB)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: (String, ALG[B])
  ): EitherData[K, ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    EitherData(Right(definitionA), typeNameOfA, Right(definitionB._2), definitionB._1)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: ALG[B]
  ): EitherData[K, ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    val typeNameOfB = manifest[B].runtimeClass.getName.split('$').lastOption.getOrElse("")
    EitherData(Right(definitionA), typeNameOfA, Right(definitionB), typeNameOfB)
  }

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](
    definitionA: HigherOrderValue[K, ALG, A],
    definitionB: HigherOrderValue[K, ALG, B]
  ): EitherData[K, ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    val typeNameOfB = manifest[B].runtimeClass.getName.split('$').lastOption.getOrElse("")
    EitherData(Left(definitionA), typeNameOfA, Left(definitionB), typeNameOfB)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: HigherOrderValue[K, ALG, A],
    definitionB: ALG[B]
  ): EitherData[K, ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    val typeNameOfB = manifest[B].runtimeClass.getName.split('$').lastOption.getOrElse("")
    EitherData(Left(definitionA), typeNameOfA, Right(definitionB), typeNameOfB)
  }

  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: HigherOrderValue[K, ALG, B]
  ): EitherData[K, ALG, A, B] = {
    val typeNameOfA = manifest[A].runtimeClass.getName.split('$').lastOption.getOrElse("")
    val typeNameOfB = manifest[B].runtimeClass.getName.split('$').lastOption.getOrElse("")
    EitherData(Right(definitionA), typeNameOfA, Left(definitionB), typeNameOfB)
  }

  def kvpNil: KvpNil[K, ALG] = KvpNil[K, ALG]()

  def kvpCoNil: KvpCoNil[K, ALG] = KvpCoNil[K, ALG]()

}
