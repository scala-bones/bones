package com.bones.data

import com.bones.PrimitiveValue
import com.bones.validation.ValidationDefinition.ValidationOp

object PrimitiveWrapperValue {
  implicit class ToCollection[ALG[_] <: PrimitiveWrapperValue[ALG, A], A: Manifest](hm: ALG[A]) {
    self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional: OptionalValue[ALG, A] =
      OptionalValue[ALG, A](Right(hm))
  }
}

sealed abstract class PrimitiveWrapperValue[ALG[_], A: Manifest] extends PrimitiveValue[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

trait Named {
  def name: String
}

trait ConcreteValueTemplate[ALG[_], OUT] {
  def fromConcreteValue[A: Manifest](kvpCollection: PrimitiveWrapperValue[ALG, A]): OUT = {
    kvpCollection match {
      case ov: OptionalValue[ALG, b] =>
        implicit val manifestOfB: Manifest[b] = ov.manifestOfB
        optionalToOut(ov)
      case ed: EitherData[ALG, a, b] =>
        implicit val manifestOfA: Manifest[a] = ed.manifestOfLeft
        implicit val manifestOfB: Manifest[b] = ed.manifestOfRight
        eitherToOut(ed)
      case ld: ListData[ALG, t] =>
        implicit val manifestOfT: Manifest[t] = ld.manifestOfT
        listToOut(ld)
      case hl: KvpCollectionValue[ALG, a] => kvpCollectionToOut(hl)
    }
  }

  protected def optionalToOut[B: Manifest](opt: OptionalValue[ALG, B]): OUT
  protected def eitherToOut[A: Manifest, B: Manifest](either: EitherData[ALG, A, B]): OUT
  protected def listToOut[A: Manifest](list: ListData[ALG, A]): OUT
  protected def kvpCollectionToOut[A](hList: KvpCollectionValue[ALG, A]): OUT

}

/** Wraps a data definition to mark the field optional */
case class OptionalValue[ALG[_], B: Manifest](
  valueDefinitionOp: Either[PrimitiveWrapperValue[ALG, B], ALG[B]])
    extends PrimitiveWrapperValue[ALG, Option[B]] {
  val manifestOfB: Manifest[B] = manifest[B]
}

final case class EitherData[ALG[_], A: Manifest, B: Manifest](
  definitionA: Either[PrimitiveWrapperValue[ALG, A], ALG[A]],
  definitionB: Either[PrimitiveWrapperValue[ALG, B], ALG[B]])
    extends PrimitiveWrapperValue[ALG, Either[A, B]] {
  val manifestOfLeft: Manifest[A] = manifest[A]
  val manifestOfRight: Manifest[B] = manifest[B]
}

/** Represents a type where the value is a List of T */
final case class ListData[ALG[_], T: Manifest](
  tDefinition: Either[PrimitiveWrapperValue[ALG, T], ALG[T]],
  validations: List[ValidationOp[List[T]]])
    extends PrimitiveWrapperValue[ALG, List[T]] {
  val manifestOfT: Manifest[T] = manifest[T]
}

/** Represents a type where the value is an KvpCollection */
final case class KvpCollectionValue[ALG[_], A: Manifest](
  kvpCollection: KvpCollection[ALG, A],
  validations: List[ValidationOp[A]])
    extends PrimitiveWrapperValue[ALG, A] {}
