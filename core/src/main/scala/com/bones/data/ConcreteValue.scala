package com.bones.data

import com.bones.PrimitiveValue
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist.Tupler
import shapeless.{Coproduct, Generic, HList, Nat}

object ConcreteValue {
  implicit class ToCollection[ALG[_] <: ConcreteValue[ALG, A], A: Manifest](hm: ALG[A]) {
    self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional: OptionalValue[ALG, A] =
      OptionalValue[ALG, A](Right(hm))
  }
}

sealed abstract class ConcreteValue[ALG[_], A: Manifest] extends PrimitiveValue[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

trait Named {
  def name: String
}

trait ConcreteValueTemplate[ALG[_], OUT] {
  def fromCollection[A: Manifest](kvpCollection: ConcreteValue[ALG, A]): OUT = {
    kvpCollection match {
      case ov: OptionalValue[ALG, b] => {
        implicit val manifestOfB = ov.manifestOfB
        optionalToOut(ov)
      }
      case ed: EitherData[ALG, a, b] => {
        implicit val manifestOfA = ed.manifestOfLeft
        implicit val manifestOfB = ed.manifestOfRight
        eitherToOut(ed)
      }
      case ld: ListData[ALG, t] => {
        implicit val manifestOfT = ld.manifestOfT
        listToOut(ld)
      }
      case hl: KvpHListValue[ALG, h, hl]   => hListToOut(hl)
      case cv: CoproductCollection[ALG, c] => coproductToOut(cv)
      case hc: Switch[ALG, h, n, A]        => hListConvertToOut(hc)
      case cc: CoproductSwitch[ALG, c, A]  => coproductConvertToOut(cc)
    }
  }

  protected def optionalToOut[B: Manifest](opt: OptionalValue[ALG, B]): OUT
  protected def eitherToOut[A: Manifest, B: Manifest](either: EitherData[ALG, A, B]): OUT
  protected def listToOut[A: Manifest](list: ListData[ALG, A]): OUT
  protected def hListToOut[H <: HList, HL <: Nat](hList: KvpHListValue[ALG, H, HL]): OUT
  protected def coproductToOut[C <: Coproduct](coproduct: CoproductCollection[ALG, C]): OUT
  protected def hListConvertToOut[A: Manifest, H <: HList, N <: Nat](
    hList: Switch[ALG, H, N, A]): OUT
  protected def coproductConvertToOut[C <: Coproduct, A: Manifest](
    cc: CoproductSwitch[ALG, C, A]): OUT

}

/** Wraps a data definition to mark the field optional */
case class OptionalValue[ALG[_], B: Manifest](
  valueDefinitionOp: Either[ConcreteValue[ALG, B], ALG[B]])
    extends ConcreteValue[ALG, Option[B]] {
  val manifestOfB = manifest[B]
}

final case class EitherData[ALG[_], A: Manifest, B: Manifest](
  definitionA: Either[ConcreteValue[ALG, A], ALG[A]],
  definitionB: Either[ConcreteValue[ALG, B], ALG[B]])
    extends ConcreteValue[ALG, Either[A, B]] {
  val manifestOfLeft = manifest[A]
  val manifestOfRight = manifest[B]
}

/** Represents a type where the value is a List of T */
final case class ListData[ALG[_], T: Manifest](
  tDefinition: Either[ConcreteValue[ALG, T], ALG[T]],
  validations: List[ValidationOp[List[T]]])
    extends ConcreteValue[ALG, List[T]] {
  val manifestOfT = manifest[T]
}

/** Represents a type where the value is an HList */
final case class KvpHListValue[ALG[_], H <: HList: Manifest, HL <: Nat](
  kvpHList: KvpCollection[ALG, H, HL],
  validations: List[ValidationOp[H]])
    extends ConcreteValue[ALG, H] {

  def convert[Z: Manifest](convertValidation: ValidationOp[Z]*)(
    implicit gen: Generic.Aux[Z, H]): Switch[ALG, H, HL, Z] =
    Switch(kvpHList, gen.from, gen.to, convertValidation.toList)

  def tupled[Tup <: Product: Manifest](tupleValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[H, Tup],
    gen: Generic[Tup]
  ): Switch[ALG, H, HL, Tup] =
    Switch[ALG, H, HL, Tup](
      kvpHList,
      (h: H) => tupler.apply(h),
      (t: Tup) => gen.to(t).asInstanceOf[H],
      tupleValidations.toList)

}

/** Represents a coproduct value where the resulting type is a shapeless coproduct */
final case class CoproductCollection[ALG[_], C <: Coproduct: Manifest](
  kvpCoproduct: KvpCoproduct[ALG, C])
    extends ConcreteValue[ALG, C]

/** Represents a switch in encoding from an HList to a type A. */
final case class Switch[ALG[_], H <: HList, N <: Nat, A: Manifest](
  from: KvpCollection[ALG, H, N],
  fHtoA: H => A,
  fAtoH: A => H,
  validations: List[ValidationOp[A]])
    extends ConcreteValue[ALG, A]

/** Represents a switch in encoding from a coproduct value C to a type A */
final case class CoproductSwitch[ALG[_], C <: Coproduct, A: Manifest](
  from: KvpCoproduct[ALG, C],
  cToA: C => A,
  aToC: A => C,
  validations: List[ValidationOp[A]]
) extends ConcreteValue[ALG, A]
