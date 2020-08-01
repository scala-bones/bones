package com.bones.data

import com.bones.KvpValue
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist.Tupler
import shapeless.{Coproduct, Generic, HList, Nat}



object KvpCollection {
  implicit class ToCollection[ALG[_] <: KvpCollection[ALG, A], A: Manifest](hm: ALG[A]) { self =>
    def list(validationOps: ValidationOp[List[A]]*): ListData[ALG, A] =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional: OptionalKvpValueDefinition[ALG, A] =
      OptionalKvpValueDefinition[ALG, A](Right(hm))
  }
}

sealed abstract class KvpCollection[ALG[_], A: Manifest] extends KvpValue[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

trait KvpCollectionTemplate[ALG[_], OUT] {
  def fromCollection[A:Manifest](kvpCollection: KvpCollection[ALG, A]): OUT = {
    kvpCollection match {
      case ov: OptionalKvpValueDefinition[ALG,b] => {
        implicit val manfifestOfB = ov.manifestOfB
        optionalToOut(ov)
      }
      case ed: EitherData[ALG,a,b] => {
        implicit val manifestOfA = ed.manifestOfLeft
        implicit val manifestOfB = ed.manifestOfRight
        eitherToOut(ed)
      }
      case ld: ListData[ALG,t] => {
        implicit val manifestOfT = ld.manifestOfT
        listToOut(ld)
      }
      case hl: KvpHListValue[ALG, h, hl] => hListToOut(hl)
      case cv: KvpCoproductValue[ALG, c] => coproductToOut(cv)
      case hc: HListConvert[ALG, h, n, A] => hListConvertToOut(hc)
      case cc: KvpCoproductConvert[ALG, c, A] => coproductConvertToOut(cc)
    }
  }

  protected def optionalToOut[B:Manifest](opt: OptionalKvpValueDefinition[ALG,B]): OUT
  protected def eitherToOut[A:Manifest,B:Manifest](either: EitherData[ALG,A,B]): OUT
  protected def listToOut[A:Manifest](list: ListData[ALG, A]): OUT
  protected def hListToOut[H<:HList,HL<:Nat](hList: KvpHListValue[ALG, H, HL]): OUT
  protected def coproductToOut[C<:Coproduct](coproduct: KvpCoproductValue[ALG,C]): OUT
  protected def hListConvertToOut[A:Manifest, H<:HList,N<:Nat](hList: HListConvert[ALG,H,N,A]): OUT
  protected def coproductConvertToOut[C<:Coproduct, A:Manifest](cc: KvpCoproductConvert[ALG,C,A]): OUT

}

/** Wraps a data definition to mark the field optional */
case class OptionalKvpValueDefinition[ALG[_], B: Manifest](
  valueDefinitionOp: Either[KvpCollection[ALG, B], ALG[B]])
    extends KvpCollection[ALG, Option[B]] {
  val manifestOfB = manifest[B]
}


final case class EitherData[ALG[_], A: Manifest, B: Manifest](
  definitionA: Either[KvpCollection[ALG, A], ALG[A]],
  definitionB: Either[KvpCollection[ALG, B], ALG[B]])
    extends KvpCollection[ALG, Either[A, B]] {
  val manifestOfLeft = manifest[A]
  val manifestOfRight = manifest[B]
}

/** Represents a type where the value is a List of T */
final case class ListData[ALG[_], T: Manifest](
  tDefinition: Either[KvpCollection[ALG, T], ALG[T]],
  validations: List[ValidationOp[List[T]]])
    extends KvpCollection[ALG, List[T]] {
  val manifestOfT = manifest[T]
}

/** Represents a type where the value is an HList */
final case class KvpHListValue[ALG[_], H <: HList: Manifest, HL <: Nat](
  kvpHList: KvpHList[ALG, H, HL],
  validations: List[ValidationOp[H]])
    extends KvpCollection[ALG, H] {

  def convert[Z: Manifest](convertValidation: ValidationOp[Z]*)(
    implicit gen: Generic.Aux[Z, H]): HListConvert[ALG, H, HL, Z] =
    HListConvert(kvpHList, gen.from, gen.to, convertValidation.toList)

  def tupled[Tup <: Product: Manifest](tupleValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[H, Tup],
    gen: Generic[Tup]
  ): HListConvert[ALG, H, HL, Tup] =
    HListConvert[ALG, H, HL, Tup](
      kvpHList,
      (h: H) => tupler.apply(h),
      (t: Tup) => gen.to(t).asInstanceOf[H],
      tupleValidations.toList)

}

/** Represents a coproduct value where the resulting type is a shapeless coproduct */
final case class KvpCoproductValue[ALG[_], C <: Coproduct: Manifest](
  kvpCoproduct: KvpCoproduct[ALG, C])
    extends KvpCollection[ALG, C]

/** Represents a conversion to and from an HList to an A (where A is most likely a Case class) */
final case class HListConvert[ALG[_], H <: HList, N <: Nat, A: Manifest](
  from: KvpHList[ALG, H, N],
  fHtoA: H => A,
  fAtoH: A => H,
  validations: List[ValidationOp[A]])
    extends KvpCollection[ALG, A]

/** Represents a coproduct value C which is to be converted to a class represented by A */
final case class KvpCoproductConvert[ALG[_], C <: Coproduct, A: Manifest](
  from: KvpCoproduct[ALG, C],
  cToA: C => A,
  aToC: A => C,
  validations: List[ValidationOp[A]]
) extends KvpCollection[ALG, A]
