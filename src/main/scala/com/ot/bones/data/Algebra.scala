package com.ot.bones.data

import java.text.{DateFormat, Format, SimpleDateFormat}
import java.util.{Date, UUID}

import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.data.HListAlgebra.BaseHListDef
import com.ot.bones.interpreter.EncoderInterpreter.Encode
import com.ot.bones.interpreter.ExtractionInterpreter.{CanNotConvert, ExtractionErrors, JsonProducer, RequiredData, ValidateFromProducer, ValidationError, ValidationResultNel}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import shapeless.{::, Generic, HList, HNil, Nat}
import HList._
import com.ot.bones.data.HListAlgebra.HListAppendN.HListPrependN
import com.ot.bones.validation.{ValidationUtil => vu}
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.ops.nat.Sum

object Algebra {

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition.. */
  sealed trait DataDefinitionOp[A] {
    //lift any DataDefinition into a FreeApplicative
    def lift: DataDefinition[A] = FreeApplicative.lift(this)

    def transform[Z:Manifest](implicit gen: Generic.Aux[Z, A]) = {
      Transform(this, gen.to _, gen.from _)
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
  final case class DateData(dateFormat: DateFormat, formatDescription: String)
    extends DataDefinitionOp[Date] with ToOptionalData[Date]
  final case class UuidData() extends DataDefinitionOp[UUID] with ToOptionalData[UUID]

  final case class ConversionData[A,B](
                                        from: DataDefinitionOp[A],
                                        fab: A => Either[CanNotConvert[A,B], B],
                                        fba: B => A, description: String
  ) extends DataDefinitionOp[B] with ToOptionalData[B]

  final case class EnumeratedStringData[A](enumeration: Enumeration) extends DataDefinitionOp[A] with ToOptionalData[A]
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

    /** Extract the child or children. */
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]

    def encodeMembers(value: FunctionK[DataDefinitionOp, Encode]): Encode[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, L] = {
      jsonProducer.produceObject.leftMap(NonEmptyList.one).andThen {
        case Some(producer) => extractMembers(functionK)(producer)
        case None => Invalid(NonEmptyList.one(RequiredData(this)))
      }.andThen { l =>
        vu.validate(l, validations)
      }
    }

    def encode(functionK: FunctionK[DataDefinitionOp, Encode]): L => JValue = {
      (l: L) => {
        encodeMembers(functionK).apply(l)
      }
    }

    def validations: List[ValidationOp[L]]

    def ::[OUT <: HList, P <: HList, N <: Nat](hHead: BaseHListDef[P])(
      implicit p: Prepend.Aux[P,L,OUT],
      lpLength: Length.Aux[P,N],
      s: Split.Aux[OUT,N,P,L]
    ) = {
      val psPrepend = (in : P :: L :: HNil) => p(in.head, in.tail.head)
      val hSplit = (in: OUT) => in.splitP[lpLength.Out]
      new HListPrependN[OUT] {
        type Prefix = P
        type Suffix = L
        override val prepend = psPrepend
        override val split = hSplit
        override val prefix: BaseHListDef[P] = hHead
        override val suffix: BaseHListDef[L] = thisBase
        override val validations = List.empty[ValidationOp[OUT]]
      }
    }

  }

  def hMember[A](op1: FieldDefinition[A], validation: ValidationOp[A :: HNil]*) : HMember[A] = {
    HMember(op1, validation.toList)
  }
  final case class HMember[A](op1: FieldDefinition[A], validations: List[ValidationOp[A :: HNil]])
    extends BaseHListDef[A :: HNil] {

    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: HNil] = {
      val r1 = functionK(op1.op)
      (jsonProducer: JsonProducer) => {
        vu.pv(jsonProducer, op1, r1).map(_ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: HNil] = input => {
      val res1 = functionK(op1.op)(input.head)
      JObject(List(JField(op1.key.name, res1)))
    }


    def members: List[FieldDefinition[_]] = List(op1)

    def validate(validationOp: ValidationOp[A :: HNil]*): HMember[A] =
      this.copy(validations = validationOp.toList)

    val hLength = Length[A :: HNil]

  }


  object HListAppendN {

    trait HListPrependN[OUTN <: HList] extends BaseHListDef[OUTN] { outer =>
      type Prefix <: HList
      type Suffix <: HList
      type Out = Prefix :: Suffix :: HNil
      val prepend: Prefix :: Suffix :: HNil => OUTN
      val split : OUTN => Prefix :: Suffix :: HNil

      val prefix: BaseHListDef[Prefix]
      val suffix: BaseHListDef[Suffix]
      val validations: List[ValidationOp[OUTN]]

      /** Extract the child or children.*/
      override def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[OUTN] = {
        val m1 = functionK.apply(prefix)
        val m2 = functionK.apply(suffix)
        (json: JsonProducer) => {
          (m1(json), m2(json)).mapN( (l1, l2) => prepend.apply(l1 :: l2 :: HNil) )
        }
      }

      def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[OUTN] = {
        (out: OUTN) => {
          val l = split(out)
          val m1 = functionK.apply(prefix).apply(l.head)
          val m2 = functionK.apply(suffix).apply(l.tail.head)
          JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
        }
      }

      /** Get a list of untyped members */
      override def members: List[FieldDefinition[_]] = prefix.members ::: suffix.members

      def validate(validationOp: ValidationOp[OUTN]*): HListPrependN[OUTN] = new HListPrependN[OUTN] {
        override type Prefix = outer.Prefix
        override type Suffix = outer.Suffix
        override val prepend = outer.prepend
        override val split = outer.split
        override val prefix = outer.prefix
        override val suffix = outer.suffix
        override val validations: List[ValidationOp[OUTN]] = validationOp.toList ::: outer.validations
      }
    }
  }
}
