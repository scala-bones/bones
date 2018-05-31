package com.bones.data

import java.time.ZonedDateTime
import java.util.UUID

import cats.data.Validated
import cats.free.FreeApplicative
import com.bones.data.Error.{CanNotConvert, ValidationError}
import com.bones.data.HListAlgebra.BaseHListDef
import com.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import shapeless.HList._
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.{::, Generic, HList, HNil, Nat}

object Algebra {

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition.. */
  sealed trait DataDefinitionOp[A] {
    //lift any DataDefinition into a FreeApplicative
    def lift: DataDefinition[A] = FreeApplicative.lift(this)

    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, A]) = {
      Transform(this, gen.to _, gen.from _)
    }

    def convert[B](fab: A => Either[CanNotConvert[A,B], B], fba: B => A, description: String, validations: List[ValidationOp[B] with ToOptionalValidation[B]]): Algebra.ConversionData[A,B] = {
      ConversionData[A,B](this, fab, fba, description)
    }

  }
  type DataDefinition[A] = FreeApplicative[DataDefinitionOp, A]

  /** Wraps a data definition to mark the field optional */
  case class OptionalDataDefinition[B](dataDefinitionOp: DataDefinitionOp[B]) extends DataDefinitionOp[Option[B]]

  /** Syntactic sugar to wrap the data definition in an Optional type.
    * Also a sort of marker interface, if this is mixed in, the field is optional. */
  trait ToOptionalData[A] extends DataDefinitionOp[A] {
    def toOption = OptionalDataDefinition(this)
  }

  final case class BooleanData() extends DataDefinitionOp[Boolean] with ToOptionalData[Boolean]
  final case class DoubleData() extends DataDefinitionOp[Double] with ToOptionalData[Double]
  final case class EitherData[A, B](definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B])
    extends DataDefinitionOp[Either[A, B]] with ToOptionalData[Either[A, B]]
  final case class IntData() extends DataDefinitionOp[Int] with ToOptionalData[Int]
  final case class ListData[T, L <: List[T]](tDefinition: DataDefinitionOp[T])
    extends DataDefinitionOp[L] with ToOptionalData[L]
  final case class StringData() extends DataDefinitionOp[String] with ToOptionalData[String]
  final case class BigDecimalFromString() extends DataDefinitionOp[BigDecimal] with ToOptionalData[BigDecimal]

  import java.time.LocalDate
  import java.time.format.DateTimeFormatter

  final case class DateData(dateFormat: DateTimeFormatter, formatDescription: String)
    extends DataDefinitionOp[ZonedDateTime] with ToOptionalData[ZonedDateTime]
  final case class UuidData() extends DataDefinitionOp[UUID] with ToOptionalData[UUID]

  final case class ConversionData[A,B](
                                        from: DataDefinitionOp[A],
                                        fab: A => Either[CanNotConvert[A,B], B],
                                        fba: B => A, description: String
  ) extends DataDefinitionOp[B] with ToOptionalData[B]

  final case class EnumerationStringData[A](enumeration: Enumeration) extends DataDefinitionOp[A] with ToOptionalData[A]
  final case class EnumStringData[A <: Enum[A]:Manifest](enums: List[A]) extends DataDefinitionOp[A] with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }
  final case class Transform[A:Manifest,B](op: DataDefinitionOp[B], f: A => B, g: B => A) extends DataDefinitionOp[A] with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }
  final case class Check[L <: HList, N <: Nat](obj: BaseHListDef[L], check: L => Validated[ValidationError[L], L])
    extends DataDefinitionOp[L] with ToOptionalData[L]


}


object HListAlgebra {

  import Algebra._

  /** Used to create a generic extract method so we can extract values from the products. */
  sealed abstract class BaseHListDef[L <: HList] extends DataDefinitionOp[L] with ToOptionalData[L] { thisBase =>

    /** Get a list of untyped members */
    def members: List[FieldDefinition[_]]

    def validations: List[ValidationOp[L]]

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

  final case class HMember[A](op1: FieldDefinition[A], validations: List[ValidationOp[A :: HNil]])
    extends BaseHListDef[A :: HNil] {

    def members: List[FieldDefinition[_]] = List(op1)

    def validate(validationOp: ValidationOp[A :: HNil]*): HMember[A] =
      this.copy(validations = validationOp.toList)

    val hLength = Length[A :: HNil]

  }


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
