package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp

object HigherOrderValue {
  def keyMapEither[K, ALG[_], B, K2](
    e: Either[HigherOrderValue[K, ALG, B], ALG[B]],
    f: K => K2
  ): Either[HigherOrderValue[K2, ALG, B], ALG[B]] = {
    e match {
      case Left(hov)  => Left(hov.keyMap(f))
      case Right(alg) => Right(alg)
    }
  }

  def algMapEither[K, ALG[_], A, ALG2[_]](
    e: Either[HigherOrderValue[K, ALG, A], ALG[A]],
    f: ALG[_] => ALG2[_]
  ): Either[HigherOrderValue[K, ALG2, A], ALG2[A]] = {
    (e match {
      case Left(hov)  => Left(hov.algMap(f))
      case Right(alg) => Right(f(alg))
    }).asInstanceOf[Either[HigherOrderValue[K, ALG2, A], ALG2[A]]]

  }
}
sealed abstract class HigherOrderValue[K, ALG[_], A] {
  def typeName: String

  def keyMap[K2](f: K => K2): HigherOrderValue[K2, ALG, A] = this match {
    case ov: OptionalValue[K, ALG, a] =>
      ov.keyMapOptionalValue(f).asInstanceOf[HigherOrderValue[K2, ALG, A]]
    case ed: EitherData[K, ALG, a, b] =>
      ed.keyMapEitherData(f).asInstanceOf[HigherOrderValue[K2, ALG, A]]
    case ld: ListData[K, ALG, t] => ld.keyMapListData(f).asInstanceOf[HigherOrderValue[K2, ALG, A]]
    case hl: KvpCollectionValue[K, ALG, a] =>
      hl.keyMapKvpCollectionValue(f).asInstanceOf[HigherOrderValue[K2, ALG, A]]
  }

  def algMap[ALG2[_]](f: ALG[_] => ALG2[_]): HigherOrderValue[K, ALG2, A] = this match {
    case ov: OptionalValue[K, ALG, a] =>
      ov.algMapOptionalValue(f).asInstanceOf[HigherOrderValue[K, ALG2, A]]
    case ed: EitherData[K, ALG, a, b] =>
      ed.algMapEitherData(f).asInstanceOf[HigherOrderValue[K, ALG2, A]]
    case ld: ListData[K, ALG, t] => ld.algMapListData(f).asInstanceOf[HigherOrderValue[K, ALG2, A]]
    case hl: KvpCollectionValue[K, ALG, a] =>
      hl.algMapKvpCollectionValue(f).asInstanceOf[HigherOrderValue[K, ALG2, A]]
  }
}

trait Named {
  def name: String
}

trait HigherOrderTemplate[K, ALG[_], OUT] {
  def fromConcreteValue[A](kvpCollection: HigherOrderValue[K, ALG, A]): OUT = {
    kvpCollection match {
      case ov: OptionalValue[K, ALG, b] =>
        optionalToOut(ov)
      case ed: EitherData[K, ALG, a, b] =>
        eitherToOut(ed)
      case ld: ListData[K, ALG, t] =>
        listToOut(ld)
      case hl: KvpCollectionValue[K, ALG, a] => kvpCollectionToOut(hl)
    }
  }

  protected def optionalToOut[B](opt: OptionalValue[K, ALG, B]): OUT
  protected def eitherToOut[A, B](either: EitherData[K, ALG, A, B]): OUT
  protected def listToOut[A](list: ListData[K, ALG, A]): OUT
  protected def kvpCollectionToOut[A](hList: KvpCollectionValue[K, ALG, A]): OUT

}

/** Wraps a data definition to mark the field optional */
case class OptionalValue[K, ALG[_], B](
  valueDefinitionOp: Either[HigherOrderValue[K, ALG, B], ALG[B]],
  typeNameOfB: String
) extends HigherOrderValue[K, ALG, Option[B]] {
  override def typeName: String = typeNameOfB

  def keyMapOptionalValue[K2](f: K => K2): OptionalValue[K2, ALG, B] =
    this.copy(valueDefinitionOp = HigherOrderValue.keyMapEither(valueDefinitionOp, f))

  def algMapOptionalValue[ALG2[_]](f: ALG[_] => ALG2[_]): OptionalValue[K, ALG2, B] =
    this.copy(
      valueDefinitionOp = HigherOrderValue.algMapEither[K, ALG, B, ALG2](valueDefinitionOp, f)
    )

}

final case class EitherData[K, ALG[_], A, B](
  definitionA: Either[HigherOrderValue[K, ALG, A], ALG[A]],
  typeNameOfA: String,
  definitionB: Either[HigherOrderValue[K, ALG, B], ALG[B]],
  typeNameOfB: String
) extends HigherOrderValue[K, ALG, Either[A, B]] {

  override def typeName: String = typeNameOfA + "Or" + typeNameOfB

  def list(validationOps: ValidationOp[List[Either[A, B]]]*): ListData[K, ALG, Either[A, B]] = {
    val typeName = typeNameOfA + "Or" + typeNameOfB.capitalize
    ListData[K, ALG, Either[A, B]](Left(this), typeName, validationOps.toList)
  }

  def optional: OptionalValue[K, ALG, Either[A, B]] = {
    val typeName = typeNameOfA + "Or" + typeNameOfB.capitalize
    OptionalValue[K, ALG, Either[A, B]](Left(this), typeName)
  }

  def keyMapEitherData[K2](f: K => K2): EitherData[K2, ALG, A, B] =
    this.copy(
      definitionA = HigherOrderValue.keyMapEither(definitionA, f),
      definitionB = HigherOrderValue.keyMapEither(definitionB, f)
    )

  def algMapEitherData[ALG2[_]](f: ALG[_] => ALG2[_]): EitherData[K, ALG2, A, B] =
    this.copy(
      definitionA = HigherOrderValue.algMapEither[K, ALG, A, ALG2](definitionA, f),
      definitionB = HigherOrderValue.algMapEither[K, ALG, B, ALG2](definitionB, f)
    )

}

/** Represents a type where the value is a List of T */
final case class ListData[K, ALG[_], T](
  tDefinition: Either[HigherOrderValue[K, ALG, T], ALG[T]],
  typeNameOfT: String,
  validations: List[ValidationOp[List[T]]]
) extends HigherOrderValue[K, ALG, List[T]] {

  override def typeName: String = "listOf" + typeNameOfT

  def list(validationOps: ValidationOp[List[List[T]]]*): ListData[K, ALG, List[T]] = {
    val typeName = "listOf" + typeNameOfT.capitalize
    ListData[K, ALG, List[T]](Left(this), typeName, validationOps.toList)
  }

  def optional: OptionalValue[K, ALG, List[T]] = {
    val typeName = "listOf" + typeNameOfT.capitalize
    OptionalValue[K, ALG, List[T]](Left(this), typeName)
  }

  def keyMapListData[K2](f: K => K2): ListData[K2, ALG, T] =
    this.copy(tDefinition = HigherOrderValue.keyMapEither(tDefinition, f))

  def algMapListData[ALG2[_]](f: ALG[_] => ALG2[_]): ListData[K, ALG2, T] =
    this.copy(tDefinition = HigherOrderValue.algMapEither[K, ALG, T, ALG2](tDefinition, f))
}

/** Represents a type where the value is an KvpCollection */
final case class KvpCollectionValue[K, ALG[_], A](
  kvpCollection: KvpCollection[K, ALG, A],
  typeName: String,
  validations: List[ValidationOp[A]]
) extends HigherOrderValue[K, ALG, A] {

  def list(validationOps: ValidationOp[List[A]]*): ListData[K, ALG, A] = {
    ListData[K, ALG, A](Left(this), typeName, validationOps.toList)
  }

  def optional: OptionalValue[K, ALG, A] = {
    OptionalValue[K, ALG, A](Left(this), typeName)
  }

  def keyMapKvpCollectionValue[KK](f: K => KK): KvpCollectionValue[KK, ALG, A] = {
    this.copy(kvpCollection = kvpCollection.keyMapKvpCollection(f))
  }

  def algMapKvpCollectionValue[ALG2[_]](f: ALG[_] => ALG2[_]): KvpCollectionValue[K, ALG2, A] = {
    this.copy(kvpCollection = kvpCollection.algMapKvpCollection(f))
  }

}
