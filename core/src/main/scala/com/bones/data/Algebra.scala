package com.bones.data

import java.time.ZonedDateTime
import java.util.UUID

import cats.data.Validated
import cats.free.FreeApplicative
import com.bones.data.Error.{CanNotConvert, ValidationError}
import com.bones.data.HListAlgebra.{BaseHListDef, HDataDefinition, HListPrependN, HMember}
import com.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.HList._
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.{::, Generic, HList, HNil, Nat}

object Algebra {

  /** ValueDefinitionOp is the base trait to describe a piece of data which may be
    * a single value or an HList. */
  sealed trait ValueDefinitionOp[A] {
    //lift any DataDefinition into a FreeApplicative
    def lift: DataDefinition[A] = ???

//    def ::[B](dataDefinitionOp: ValueDefinitionOp[B]): HListPrependN[B::A::HNil, B::HNil, A::HNil] = {
//      HDataDefinition[B](dataDefinitionOp) :: HDataDefinition[A](this)
//    }

    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, A]) = {
      Transform(this, gen.to _, gen.from _)
    }

    def convert[B](fab: A => Either[CanNotConvert[A,B], B], fba: B => A, description: String, validations: List[ValidationOp[B] with ToOptionalValidation[B]]): Algebra.ConversionData[A,B] = {
      ConversionData[A,B](this, fab, fba, description)
    }

  }

  type DataDefinition[A] = FreeApplicative[ValueDefinitionOp, A]

  /** Wraps a data definition to mark the field optional */
  case class OptionalDataDefinition[B](dataDefinitionOp: ValueDefinitionOp[B]) extends ValueDefinitionOp[Option[B]]

  /** Syntactic sugar to wrap the data definition in an Optional type.
    * Also a sort of marker interface, if this is mixed in, the field is optional. */
  trait ToOptionalData[A] extends ValueDefinitionOp[A] {
    def toOption = OptionalDataDefinition(this)
  }

  final case class BooleanData() extends ValueDefinitionOp[Boolean] with ToOptionalData[Boolean]
  final case class DoubleData() extends ValueDefinitionOp[Double] with ToOptionalData[Double]
  final case class EitherData[A, B](definitionA: ValueDefinitionOp[A], definitionB: ValueDefinitionOp[B])
    extends ValueDefinitionOp[Either[A, B]] with ToOptionalData[Either[A, B]]
  final case class IntData() extends ValueDefinitionOp[Int] with ToOptionalData[Int]
  final case class ListData[T, L <: List[T]](tDefinition: ValueDefinitionOp[T])
    extends ValueDefinitionOp[L] with ToOptionalData[L]
  final case class StringData() extends ValueDefinitionOp[String] with ToOptionalData[String]
  final case class BigDecimalFromString() extends ValueDefinitionOp[BigDecimal] with ToOptionalData[BigDecimal]

  import java.time.format.DateTimeFormatter

  final case class DateData(dateFormat: DateTimeFormatter, formatDescription: String)
    extends ValueDefinitionOp[ZonedDateTime] with ToOptionalData[ZonedDateTime]
  final case class UuidData() extends ValueDefinitionOp[UUID] with ToOptionalData[UUID]

  final case class ConversionData[A,B](
                                        from: ValueDefinitionOp[A],
                                        fab: A => Either[CanNotConvert[A,B], B],
                                        fba: B => A, description: String
  ) extends ValueDefinitionOp[B] with ToOptionalData[B]

  final case class EnumerationStringData[A](enumeration: Enumeration) extends ValueDefinitionOp[A] with ToOptionalData[A]
  final case class EnumStringData[A <: Enum[A]:Manifest](enums: List[A]) extends ValueDefinitionOp[A] with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }
  final case class Transform[A:Manifest,B](op: ValueDefinitionOp[B], f: A => B, g: B => A) extends ValueDefinitionOp[A] with ToOptionalData[A] { thisBase =>
    val manifestOfA: Manifest[A] = manifest[A]
  }

  final case class Check[L <: HList, N <: Nat](obj: BaseHListDef[L], check: L => Validated[ValidationError[L], L])
    extends ValueDefinitionOp[L] with ToOptionalData[L]


}

object KvpAlgebra {
  import Algebra._

  sealed trait KvpGroup[L <: HList] extends ValueDefinitionOp[L] with ToOptionalData[L] { self =>
    def ::[H](h: KeyValueDefinition[H], validations: List[ValidationOp[H]]): KvpGroup[H :: L] =
      KvpSingleHead(h,validations,self)

    /**
      *
      * @param kvp
      * @tparam OUT New HList which combines L (from this) and P (from others)
      * @tparam P
      */
    def :::[OUT <: HList, P <: HList](kvp: KvpGroup[P]): KvpGroup[OUT] =
      KvpListHead(kvp, ???, ???, List.empty, self)
  }

  final case class KvpNil() extends KvpGroup[HNil] {

    override def :::[OUT <: HList, P <: HList](kvp: KvpGroup[P]): KvpGroup[P] = ???
  }

  final case class KvpSingleHead[H, T <: HList](
    fieldDefinition: KeyValueDefinition[H],
    validation: List[ValidationOp[H]],
    tail: KvpGroup[T]
  ) extends KvpGroup[H :: T] {

  }

  /** This is a group of KvpGroup that are grouped and the validations match the entire group.  */
  final case class KvpListHead[OUT <: HList, H <: HList, HL<: Nat, T <: HList](
                                                                                head: KvpGroup[H],
                                                                                prepend: H :: T :: HNil => OUT, // analogous to prepend : Prepend[H, T],
                                                                                split : OUT => H :: T :: HNil, // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
                                                                                validation: List[ValidationOp[H]],
                                                                                tail: KvpGroup[T]
  ) extends KvpGroup[OUT] {

  }


}


object HListAlgebra {

  import Algebra._

  /** Used to create a generic extract method so we can extract values from the products. */
  sealed abstract class BaseHListDef[L <: HList] extends ValueDefinitionOp[L] with ToOptionalData[L] { thisBase =>


    /** Get a list of untyped members */
    def members: List[FieldDefinition[_]]

    def validations: List[ValidationOp[L]]

    def ::[A](op: ValueDefinitionOp[A])(
      implicit p: Prepend.Aux[A::HNil,L,A :: L],
      lpLength: Length.Aux[A::HNil,Nat._1],
      s: Split.Aux[A :: L,Nat._1,A::HNil,L]
    ) : HListPrependN[A :: L, A :: HNil,L] = {
      val psPrepend = (in : (A::HNil) :: L :: HNil) => p(in.head, in.tail.head)
      val hSplit = (in: A::L) => in.splitP[lpLength.Out]
      val head = HDataDefinition(op)
      HListPrependN[A::L, A::HNil, L](psPrepend, hSplit, head, thisBase, List.empty)
    }

    def ::[OUT <: HList, P <: HList, N <: Nat](hHead: BaseHListDef[P])(
      implicit p: Prepend.Aux[P,L,OUT],
      lpLength: Length.Aux[P,N],
      s: Split.Aux[OUT,N,P,L]
    ) = {
      val psPrepend = (in : P :: L :: HNil) => p(in.head, in.tail.head)
      val hSplit = (in: OUT) => in.splitP[lpLength.Out]
      HListPrependN[OUT, P, L](psPrepend, hSplit, hHead, thisBase, List.empty)
    }

  }

  /** Allows us to wrap any ValueDefinitionOp into a BaseHListDef so we can append the op to a BaseHList */
  final case class HDataDefinition[A](op: ValueDefinitionOp[A]) extends BaseHListDef[A::HNil] {
    /** Get a list of untyped members */
    override def members: List[FieldDefinition[_]] = List.empty

    override def validations: List[ValidationOp[A :: HNil]] = List.empty
  }

  final case class HMember[A](op1: FieldDefinition[A], validations: List[ValidationOp[A :: HNil]])
    extends BaseHListDef[A :: HNil] {

    def members: List[FieldDefinition[_]] = List(op1)

    def validate(validationOp: ValidationOp[A :: HNil]*): HMember[A] =
      this.copy(validations = validationOp.toList)

    val hLength = Length[A :: HNil]

  }

  /** Represents prepending any BaseHListDef together.
    * We can combine two HLists.  Each HList will maintain it's own validations.
    *
    * */
  case class HListPrependN[OUTN <: HList, Prefix <: HList, Suffix <: HList](
    prepend: Prefix :: Suffix :: HNil => OUTN,
    split : OUTN => Prefix :: Suffix :: HNil,
    prefix: BaseHListDef[Prefix],
    suffix: BaseHListDef[Suffix],
    validations: List[ValidationOp[OUTN]]) extends BaseHListDef[OUTN] {
    outer =>
    type Out = Prefix :: Suffix :: HNil

    /** Get a list of untyped members */
    override def members: List[FieldDefinition[_]] = prefix.members ::: suffix.members

    def validate(validationOp: ValidationOp[OUTN]*): HListPrependN[OUTN, Prefix, Suffix] =
      this.copy(validations = validationOp.toList ::: this.validations)

  }
}
