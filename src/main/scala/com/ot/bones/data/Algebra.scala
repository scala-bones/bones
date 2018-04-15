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
import com.ot.bones.interpreter.ExtractionInterpreter.{CanNotConvert, ExtractionErrors, JsonProducer, RequiredObjectError, ValidateFromProducer, ValidationError, ValidationResultNel}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import shapeless.{::, Generic, HList, HNil, Nat}
import HList._
import com.ot.bones.validation.{ValidationUtil => vu}
import shapeless.ops.hlist.{Prepend, Split}
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
                                        fab: A => Validated[CanNotConvert[A,B], B],
                                        fba: B => A, description: String
  ) extends DataDefinitionOp[B] with ToOptionalData[B]

  final case class EnumeratedStringData[A](enumeration: Enumeration) extends DataDefinitionOp[A] with ToOptionalData[A]
  final case class Transform[A:Manifest,B](op: DataDefinitionOp[B], f: A => B, g: B => A) extends DataDefinitionOp[A] with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }
  final case class Check[L <: HList, N <: Nat](obj: BaseHListDef[L,N], check: L => Validated[ValidationError[L], L])
    extends DataDefinitionOp[L] with ToOptionalData[L]


}


object HListAlgebra {

  import Algebra._

  /** Used to create a generic extract method so we can extract values from the products. */
  sealed abstract class BaseHListDef[L <: HList, N <: Nat] extends DataDefinitionOp[L] with ToOptionalData[L] { thisBase =>

    /** Get a list of untyped members */
    def members: List[FieldDefinition[_]]

    /** Extract the child or children. */
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]

    def encodeMembers(value: FunctionK[DataDefinitionOp, Encode]): Encode[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, L] = {
      jsonProducer.produceObject.leftMap(NonEmptyList.one).andThen {
        case Some(producer) => extractMembers(functionK)(producer)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
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

    /**
      *
      * @param obj
      * @param p
      * @tparam LP HList being prepended
      * @tparam NP Out Nat which is sum LP and L
      */
    case class Delayed2Prepend[LP <: HList, NP <: Nat, L <: HList, N <: Nat](
      obj: BaseHListDef[LP, NP],
      base: BaseHListDef[L,N],
      p: Prepend[LP, L],
      sum: Sum[NP, N]
    ) {

      def :!!:[LP3 <: HList, NP3<: Nat](obj3: BaseHListDef[LP3, NP3])(
        implicit s: Split[p.Out, NP], sum3 : Sum[NP3, sum.Out], p3: Prepend[LP3, p.Out]
      ) = {
        val prepend = closePrepend(s)
        prepend.delayedPrepend(obj3)(p3, sum3)
      }

      def closePrepend(implicit s: Split[p.Out, NP]) = {
        val merge = (in : LP :: L :: HNil) => in.tail.head.:::(in.head)(p)
        val split = (in: p.Out) => in.splitP[NP].asInstanceOf[LP :: L :: HNil]
        HListAppend2[LP, NP, L, N, p.Out, sum.Out](obj, base, merge, split)
      }

      def apply(implicit s: Split[p.Out, NP]) = closePrepend(s)

    }


    /** Can't quite get this working.  This is a non-working prepend method which will hopefully
      * take the place of the current def :: in each of the subclasses. */
    def :!!:[LP <: HList, NP <: Nat](obj: BaseHListDef[LP, NP])(
      implicit p: Prepend[LP, L], sum: Sum[NP, N]) =
      Delayed2Prepend[LP, NP, L, N](obj, thisBase, p, sum)

  }

  /** Represents a required HList with two properties A and B */
  final case class HList2[A, B](
                                 op1: FieldDefinition[A],
                                 op2: FieldDefinition[B],
                                 validations: List[ValidationOp[A :: B :: HNil]]
                               ) extends BaseHListDef[A :: B :: HNil, Nat._2] {


    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: HNil] = {
      val r1 = functionK(op1.op)
      val r2 = functionK(op2.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2))
          .mapN(_ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: HNil] = input => {
      val res1 = functionK(op1.op)(input.head)
      val res2 = functionK(op2.op)(input.tail.head)
      JObject(List(JField(op1.key.name, res1), JField(op2.key.name, res2)))
    }


    def members: List[FieldDefinition[_]] = List(op1, op2)

    def validate(validationOp: ValidationOp[A :: B :: HNil]*): HList2[A, B] =
      this.copy(validations = validationOp.toList)


    def ::[A2, B2](other: HList2[A2, B2]) = {
      val merge = (i: (A2 :: B2 :: HNil) :: (A :: B :: HNil) :: HNil) => i.head ::: i.tail.head
      val split = (in: A2 :: B2 :: A :: B :: HNil) => in.splitP(Nat._2)
      HListAppend2[A2 :: B2 :: HNil, Nat._2, A :: B :: HNil, Nat._2, A2 :: B2 :: A :: B :: HNil, Nat._4](other, this, merge, split)
    }

  }


  /** Represents a required HList with three properties A,B,C */
  final case class HList3[A, B, C](
                                    op1: FieldDefinition[A],
                                    op2: FieldDefinition[B],
                                    op3: FieldDefinition[C],
                                    validations: List[ValidationOp[A :: B :: C :: HNil]]
                                  )
    extends BaseHListDef[A :: B :: C :: HNil, Nat._3] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3))
          .mapN(_ :: _ :: _ :: HNil)
      }

    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      (input: (A :: B :: C :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)), JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3)

  }

  /** Represents a required HList with four properties A,B,C,D */
  final case class HList4[A, B, C, D](
                                       op1: FieldDefinition[A],
                                       op2: FieldDefinition[B],
                                       op3: FieldDefinition[C],
                                       op4: FieldDefinition[D],
                                       validations: List[ValidationOp[A :: B :: C :: D :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: HNil, Nat._4] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4)).mapN(_ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      (input: (A :: B :: C :: D :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)), JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)), JField(op4.key.name, res4(input.tail.tail.tail.head))
        ))
      }
    }


    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4)
  }


  /** Represents a required HList with four properties. */
  final case class HList5[A, B, C, D, E](
                                          op1: FieldDefinition[A],
                                          op2: FieldDefinition[B],
                                          op3: FieldDefinition[C],
                                          op4: FieldDefinition[D],
                                          op5: FieldDefinition[E],
                                          validations: List[ValidationOp[A :: B :: C :: D :: E :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: HNil, Nat._5] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5)).mapN(_ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      (input: (A :: B :: C :: D :: E :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)), JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)), JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op4)

    def ::[A2, B2](other: HList2[A2, B2]) = {
      val merge = (i: (A2 :: B2 :: HNil) :: (A :: B :: C :: D :: E :: HNil) :: HNil) => i.head ::: i.tail.head
      val split = (in: A2 :: B2 :: A :: B :: C :: D :: E :: HNil) => in.splitP(Nat._2)
      HListAppend2[A2 :: B2 :: HNil, Nat._2, A :: B :: C :: D :: E :: HNil, Nat._5, A2 :: B2 :: A :: B :: C :: D :: E :: HNil, Nat._7](other, this, merge, split)
    }

  }


  /** Represents a required HList with six properties. */
  final case class HList6[A, B, C, D, E, F](
                                             op1: FieldDefinition[A],
                                             op2: FieldDefinition[B],
                                             op3: FieldDefinition[C],
                                             op4: FieldDefinition[D],
                                             op5: FieldDefinition[E],
                                             op6: FieldDefinition[F],
                                             validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: HNil, Nat._6] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      (input: (A :: B :: C :: D :: E :: F :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)), JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6)

  }


  /** Represents a required HList with six properties. */
  final case class HList7[A, B, C, D, E, F, G](
                                                op1: FieldDefinition[A],
                                                op2: FieldDefinition[B],
                                                op3: FieldDefinition[C],
                                                op4: FieldDefinition[D],
                                                op5: FieldDefinition[E],
                                                op6: FieldDefinition[F],
                                                op7: FieldDefinition[G],
                                                validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: G :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: G :: HNil, Nat._7] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      val r7 = f(op7.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6),
          vu.pv(jsonProducer, op7, r7)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: G :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)),
          JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)),
          JField(op7.key.name, res7(input.tail.tail.tail.tail.tail.tail.head))))
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6, op7)

    def ::[A2, B2, C2, D2, E2](other: HList5[A2, B2, C2, D2, E2]) = {
      type IN1 = A2 :: B2 :: C2 :: D2 :: E2 :: HNil
      type OUT = A2 :: B2 :: C2 :: D2 :: E2 :: A :: B :: C :: D :: E :: F :: G :: HNil
      val f = (i : IN1 :: (A :: B :: C :: D :: E :: F :: G :: HNil) :: HNil) => {
        i.head ::: i.tail.head
      }
      val split = (out: OUT) => out.splitP(5)
      HListAppend2[IN1, Nat._5, A :: B :: C :: D :: E :: F :: G :: HNil, Nat._7, OUT, Nat._12](other, this, f, split)
    }


  }


  /** Represents an HList with eight properties. */
  final case class HList8[A, B, C, D, E, F, G, H](
                                                   op1: FieldDefinition[A],
                                                   op2: FieldDefinition[B],
                                                   op3: FieldDefinition[C],
                                                   op4: FieldDefinition[D],
                                                   op5: FieldDefinition[E],
                                                   op6: FieldDefinition[F],
                                                   op7: FieldDefinition[G],
                                                   op8: FieldDefinition[H],
                                                   validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: G :: H :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: G :: H :: HNil, Nat._8] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      val r7 = f(op7.op)
      val r8 = f(op8.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6),
          vu.pv(jsonProducer, op7, r7), vu.pv(jsonProducer, op8, r8)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: G :: H :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      val res8 = functionK(op8.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)),
          JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)),
          JField(op7.key.name, res7(input.tail.tail.tail.tail.tail.tail.head)),
          JField(op8.key.name, res8(input.tail.tail.tail.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8)

  }

  /** Represents a required HList with six properties. */
  final case class HList9[A, B, C, D, E, F, G, H, I](
                                                      op1: FieldDefinition[A],
                                                      op2: FieldDefinition[B],
                                                      op3: FieldDefinition[C],
                                                      op4: FieldDefinition[D],
                                                      op5: FieldDefinition[E],
                                                      op6: FieldDefinition[F],
                                                      op7: FieldDefinition[G],
                                                      op8: FieldDefinition[H],
                                                      op9: FieldDefinition[I],
                                                      validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil, Nat._9] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      val r7 = f(op7.op)
      val r8 = f(op8.op)
      val r9 = f(op9.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6),
          vu.pv(jsonProducer, op7, r7), vu.pv(jsonProducer, op8, r8), vu.pv(jsonProducer, op9, r9)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      val res8 = functionK(op8.op)
      val res9 = functionK(op9.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)),
          JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)),
          JField(op7.key.name, res7(input.tail.tail.tail.tail.tail.tail.head)),
          JField(op8.key.name, res8(input.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op9.key.name, res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9)
  }

  /** Represents a required HList with six properties. */
  final case class HList10[A, B, C, D, E, F, G, H, I, J](
                                                          op1: FieldDefinition[A],
                                                          op2: FieldDefinition[B],
                                                          op3: FieldDefinition[C],
                                                          op4: FieldDefinition[D],
                                                          op5: FieldDefinition[E],
                                                          op6: FieldDefinition[F],
                                                          op7: FieldDefinition[G],
                                                          op8: FieldDefinition[H],
                                                          op9: FieldDefinition[I],
                                                          op10: FieldDefinition[J],
                                                          validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil, Nat._10] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      val r7 = f(op7.op)
      val r8 = f(op8.op)
      val r9 = f(op9.op)
      val r10 = f(op10.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6),
          vu.pv(jsonProducer, op7, r7), vu.pv(jsonProducer, op8, r8), vu.pv(jsonProducer, op9, r9),
          vu.pv(jsonProducer, op10, r10)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      val res8 = functionK(op8.op)
      val res9 = functionK(op9.op)
      val res10 = functionK(op10.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)),
          JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)),
          JField(op7.key.name, res7(input.tail.tail.tail.tail.tail.tail.head)),
          JField(op8.key.name, res8(input.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op9.key.name, res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op10.key.name, res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)
  }


  /** Represents a required HList with six properties. */
  final case class HList11[A, B, C, D, E, F, G, H, I, J, K](
                                                             op1: FieldDefinition[A],
                                                             op2: FieldDefinition[B],
                                                             op3: FieldDefinition[C],
                                                             op4: FieldDefinition[D],
                                                             op5: FieldDefinition[E],
                                                             op6: FieldDefinition[F],
                                                             op7: FieldDefinition[G],
                                                             op8: FieldDefinition[H],
                                                             op9: FieldDefinition[I],
                                                             op10: FieldDefinition[J],
                                                             op11: FieldDefinition[K],
                                                             validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil, Nat._11] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      val r7 = f(op7.op)
      val r8 = f(op8.op)
      val r9 = f(op9.op)
      val r10 = f(op10.op)
      val r11 = f(op11.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6),
          vu.pv(jsonProducer, op7, r7), vu.pv(jsonProducer, op8, r8), vu.pv(jsonProducer, op9, r9),
          vu.pv(jsonProducer, op10, r10), vu.pv(jsonProducer, op11, r11)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      val res8 = functionK(op8.op)
      val res9 = functionK(op9.op)
      val res10 = functionK(op10.op)
      val res11 = functionK(op11.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)),
          JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)),
          JField(op7.key.name, res7(input.tail.tail.tail.tail.tail.tail.head)),
          JField(op8.key.name, res8(input.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op9.key.name, res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op10.key.name, res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op11.key.name, res11(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)
  }


  /** Represents a required HList with six properties. */
  final case class HList12[A, B, C, D, E, F, G, H, I, J, K, L](
                                                                op1: FieldDefinition[A],
                                                                op2: FieldDefinition[B],
                                                                op3: FieldDefinition[C],
                                                                op4: FieldDefinition[D],
                                                                op5: FieldDefinition[E],
                                                                op6: FieldDefinition[F],
                                                                op7: FieldDefinition[G],
                                                                op8: FieldDefinition[H],
                                                                op9: FieldDefinition[I],
                                                                op10: FieldDefinition[J],
                                                                op11: FieldDefinition[K],
                                                                op12: FieldDefinition[L],
                                                                validations: List[ValidationOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil]])
    extends BaseHListDef[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil, Nat._12] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      val r7 = f(op7.op)
      val r8 = f(op8.op)
      val r9 = f(op9.op)
      val r10 = f(op10.op)
      val r11 = f(op11.op)
      val r12 = f(op12.op)
      (jsonProducer: JsonProducer) => {
        (vu.pv(jsonProducer, op1, r1), vu.pv(jsonProducer, op2, r2), vu.pv(jsonProducer, op3, r3),
          vu.pv(jsonProducer, op4, r4), vu.pv(jsonProducer, op5, r5), vu.pv(jsonProducer, op6, r6),
          vu.pv(jsonProducer, op7, r7), vu.pv(jsonProducer, op8, r8), vu.pv(jsonProducer, op9, r9),
          vu.pv(jsonProducer, op10, r10), vu.pv(jsonProducer, op11, r11), vu.pv(jsonProducer, op12, r12)
        ).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      val res8 = functionK(op8.op)
      val res9 = functionK(op9.op)
      val res10 = functionK(op10.op)
      val res11 = functionK(op11.op)
      val res12 = functionK(op12.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil)) => {
        JObject(List(JField(op1.key.name, res1(input.head)),
          JField(op2.key.name, res2(input.tail.head)),
          JField(op3.key.name, res3(input.tail.tail.head)),
          JField(op4.key.name, res4(input.tail.tail.tail.head)),
          JField(op5.key.name, res5(input.tail.tail.tail.tail.head)),
          JField(op6.key.name, res6(input.tail.tail.tail.tail.tail.head)),
          JField(op7.key.name, res7(input.tail.tail.tail.tail.tail.tail.head)),
          JField(op8.key.name, res8(input.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op9.key.name, res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op10.key.name, res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op11.key.name, res11(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)),
          JField(op12.key.name, res12(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)))
        )
      }
    }

    def members: List[FieldDefinition[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)
  }



  // Append Algebra, can append multiple HList types together.

  case class HListAppend2[L1 <: HList, N1 <: Nat, L2 <: HList, N2 <: Nat, OUT <: HList, NOUT <: Nat](
    h1: BaseHListDef[L1, N1],
    h2: BaseHListDef[L2, N2],
    f: L1 :: L2 :: HNil => OUT,
    f2: OUT => L1 :: L2 :: HNil
  )
    extends DataDefinitionOp[OUT] { thisAppend =>

    /** Extract the child or children.*/
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[OUT] = {
      val m1 = functionK.apply(h1)
      val m2 = functionK.apply(h2)
      (json: JsonProducer) => {
        (m1(json), m2(json)).mapN( (l1, l2) => f.apply(l1 :: l2 :: HNil) )
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[OUT] = {
      (out: OUT) => {
        val l = f2(out)
        val m1 = functionK.apply(h1).apply(l.head)
        val m2 = functionK.apply(h2).apply(l.tail.head)
        JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
      }
    }

    case class Delayed3Prepend[LP <: HList, OUT3 <: HList, NP <: Nat](obj: BaseHListDef[LP, NP], p: Prepend[LP, OUT]) {

      def apply(implicit s: Split[p.Out, NP], sum : Sum[NP, NOUT]) = {
        val merge = (in : LP :: OUT :: HNil) => in.tail.head.:::(in.head)(p)
        val split = (in: p.Out) => in.splitP[NP].asInstanceOf[LP :: OUT :: HNil]
        HListAppend3[LP, NP, L1, N1, L2, N2, OUT, NOUT, p.Out, sum.Out](obj, thisAppend, merge, split)
      }
    }


    /** This is part of an attempt to generalize :: */
    def delayedPrepend[LP <: HList, NP <: Nat](obj: BaseHListDef[LP, NP])(
      implicit p: Prepend[LP, OUT], s: Sum[NP, NOUT]) =
      Delayed3Prepend[LP, OUT, NP](obj, p)

    def ::[A2, B2, C2, E2, F2](obj: HList5[A2, B2, C2, E2, F2])(
      implicit p: Prepend[A2 :: B2 :: C2 :: E2 :: F2 :: HNil, OUT],
      sum: Sum[Nat._5, NOUT]) = {
      val merge = (i: (A2 :: B2 :: C2 :: E2 :: F2 :: HNil) :: OUT :: HNil) => i.head ::: i.tail.head
      val split = (in: p.Out) => in.asInstanceOf[A2 :: B2 :: C2 :: E2 :: F2 :: OUT].splitP(Nat._5)
      HListAppend3[A2 :: B2 :: C2 :: E2 :: F2 :: HNil, Nat._5, L1, N1, L2, N2, OUT, NOUT, p.Out, sum.Out](obj, this, merge, split)
    }
  }

  case class HListAppend3[L1 <: HList, N1 <:Nat, L2 <: HList, N2 <: Nat, L3 <: HList, N3 <: Nat, OUT2 <: HList, NOUT2 <: Nat, OUT3 <: HList, NOUT3 <: Nat]
    (
      h1: BaseHListDef[L1, N1],
      hListAppend2: HListAppend2[L2, N2, L3, N3, OUT2, NOUT2],
      f: L1 :: OUT2 :: HNil => OUT3,
      f2: OUT3 => L1 :: OUT2 :: HNil)
    extends DataDefinitionOp[OUT3] {

    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[OUT3] = {
      val m1 = functionK.apply(h1)
      val m2 = functionK.apply(hListAppend2)
      (json: JsonProducer) => {
        (m1(json), m2(json)).mapN( (l1, l2) => f.apply(l1 :: l2 :: HNil))
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[OUT3] = {
      (out: OUT3) => {
        val l = f2(out)
        val m1 = functionK.apply(h1).apply(l.head)
        val m2 = functionK.apply(hListAppend2).apply(l.tail.head)
        JObject(m1.asInstanceOf[JObject].obj ::: m2.asInstanceOf[JObject].obj)
      }
    }
  }

}
