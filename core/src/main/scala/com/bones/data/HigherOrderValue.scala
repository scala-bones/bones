package com.bones.data

import com.bones.validation.ValidationDefinition.ValidationOp

sealed abstract class HigherOrderValue[ALG[_], A] {
  def typeName: String
}

trait Named {
  def name: String
}

trait HigherOrderTemplate[K, ALG[_], OUT] {
  def fromConcreteValue[A](kvpCollection: HigherOrderValue[ALG, A]): OUT = {
    kvpCollection match {
      case ov: OptionalValue[ALG, b] =>
        optionalToOut(ov)
      case ed: EitherData[ALG, a, b] =>
        eitherToOut(ed)
      case ld: ListData[ALG, t] =>
        listToOut(ld)
      case hl: KvpCollectionValue[K, ALG, a] => kvpCollectionToOut(hl)
    }
  }

  protected def optionalToOut[B](opt: OptionalValue[ALG, B]): OUT
  protected def eitherToOut[A, B](either: EitherData[ALG, A, B]): OUT
  protected def listToOut[A](list: ListData[ALG, A]): OUT
  protected def kvpCollectionToOut[A](hList: KvpCollectionValue[K, ALG, A]): OUT

}

/** Wraps a data definition to mark the field optional */
case class OptionalValue[ALG[_], B](
  valueDefinitionOp: Either[HigherOrderValue[ALG, B], ALG[B]],
  typeNameOfB: String)
    extends HigherOrderValue[ALG, Option[B]] {
  override def typeName: String = typeNameOfB
}

final case class EitherData[ALG[_], A, B](
  definitionA: Either[HigherOrderValue[ALG, A], ALG[A]],
  typeNameOfA: String,
  definitionB: Either[HigherOrderValue[ALG, B], ALG[B]],
  typeNameOfB: String)
    extends HigherOrderValue[ALG, Either[A, B]] {

  override def typeName: String = typeNameOfA + "Or" + typeNameOfB

  def list(validationOps: ValidationOp[List[Either[A, B]]]*): ListData[ALG, Either[A, B]] = {
    val typeName = typeNameOfA + "Or" + typeNameOfB.capitalize
    ListData[ALG, Either[A, B]](Left(this), typeName, validationOps.toList)
  }

  def optional: OptionalValue[ALG, Either[A, B]] = {
    val typeName = typeNameOfA + "Or" + typeNameOfB.capitalize
    OptionalValue[ALG, Either[A, B]](Left(this), typeName)
  }

}

/** Represents a type where the value is a List of T */
final case class ListData[ALG[_], T](
  tDefinition: Either[HigherOrderValue[ALG, T], ALG[T]],
  typeNameOfT: String,
  validations: List[ValidationOp[List[T]]])
    extends HigherOrderValue[ALG, List[T]] {

  override def typeName: String = "listOf" + typeNameOfT

  def list(validationOps: ValidationOp[List[List[T]]]*): ListData[ALG, List[T]] = {
    val typeName = "listOf" + typeNameOfT.capitalize
    ListData[ALG, List[T]](Left(this), typeName, validationOps.toList)
  }

  def optional: OptionalValue[ALG, List[T]] = {
    val typeName = "listOf" + typeNameOfT.capitalize
    OptionalValue[ALG, List[T]](Left(this), typeName)
  }
}

/** Represents a type where the value is an KvpCollection */
final case class KvpCollectionValue[K, ALG[_], A](
  kvpCollection: KvpCollection[K, ALG, A],
  typeName: String,
  validations: List[ValidationOp[A]])
    extends HigherOrderValue[ALG, A] {

  def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] = {
    ListData[ALG, A](Left(this), typeName, validationOps.toList)
  }

  def optional: OptionalValue[ALG, A] = {
    OptionalValue[ALG, A](Left(this), typeName)
  }
}
