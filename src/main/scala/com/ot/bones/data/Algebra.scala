package com.ot.bones.data

import java.text.{DateFormat, Format, SimpleDateFormat}
import java.util.{Date, UUID}

import cats.arrow.FunctionK
import cats.data.Validated.Invalid
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.interpreter.EncoderInterpreter.Encode
import com.ot.bones.interpreter.ExtractionInterpreter.{ConversionError, ExtractionErrors, JsonProducer, RequiredObjectError, ValidateFromProducer}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import shapeless.{::, Generic, HList, HNil}

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

  /** Wraps a data definition to insinuate that the field is optional */
  case class OptionalDataDefinition[B](dataDefinitionOp: DataDefinitionOp[B]) extends DataDefinitionOp[Option[B]]

  /** Syntactic sugar to wrap the data definition in an Optional type */
  trait ToOptionalData[A] extends DataDefinitionOp[A] {
    def toOption = OptionalDataDefinition(this)
  }

  final case class BooleanData() extends DataDefinitionOp[Boolean] with ToOptionalData[Boolean]
  final case class DoubleData() extends DataDefinitionOp[Double] with ToOptionalData[Double]
  final case class EitherData[A, B](definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B])
    extends DataDefinitionOp[Either[A, B]] with ToOptionalData[Either[A, B]]
  final case class IntData() extends DataDefinitionOp[Int] with ToOptionalData[Int]
  final case class ListData[T, L <: List[T]](tDefinition: DataDefinitionOp[T]) extends DataDefinitionOp[L] with ToOptionalData[L]
  final case class StringData() extends DataDefinitionOp[String] with ToOptionalData[String]
  final case class BigDecimalFromString() extends DataDefinitionOp[BigDecimal] with ToOptionalData[BigDecimal]
  final case class DateData(dateFormat: DateFormat, formatDescription: String) extends DataDefinitionOp[Date] with ToOptionalData[Date]
  final case class UuidData() extends DataDefinitionOp[UUID] with ToOptionalData[UUID]
  final case class ConversionData[A,B](from: DataDefinitionOp[A], fab: A => Validated[ConversionError[A,B], B], fba: B => A, description: String) extends DataDefinitionOp[B] with ToOptionalData[B]
  final case class EnumeratedStringData[A](enumeration: Enumeration) extends DataDefinitionOp[A] with ToOptionalData[A]
  final case class Transform[A:Manifest,B](op: DataDefinitionOp[B], f: A => B, g: B => A) extends DataDefinitionOp[A] with ToOptionalData[A] {
    val manifestOfA: Manifest[A] = manifest[A]
  }

}


object ToHList {
  import Algebra._

  /** Used to create a generic extract method so we can extract values from the products. */
  abstract class ToHListDataDefinitionOp[L <: HList] extends DataDefinitionOp[L] with ToOptionalData[L] {

    /** Get a list of untyped members */
    def members: List[FieldDefinition[_]]

    /** Extract the child or children.  AA should be a Tuple */
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]

    def encodeMembers(value: FunctionK[DataDefinitionOp, Encode]): Encode[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, L] = {
      jsonProducer.produceObject.leftMap(NonEmptyList.one).andThen {
        case Some(producer) => extractMembers(functionK)(producer)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }

    def encode(functionK: FunctionK[DataDefinitionOp, Encode]): L => JValue = {
      (l: L) => {
        encodeMembers(functionK).apply(l)
      }
    }

  }

  /** Represents a required HList with two properties A and B */
  final case class HList2[A, B](
                                 op1: FieldDefinition[A],
                                 op2: FieldDefinition[B]
                               ) extends ToHListDataDefinitionOp[A :: B :: HNil] {

    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      (jsonProducer: JsonProducer) => {
        (res1(jsonProducer.child(op1.key)), res2(jsonProducer.child(op2.key)))
          .mapN(_ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, Encode]): Encode[A :: B :: HNil] = input => {
      val res1 = functionK(op1.op)(input.head)
      val res2 = functionK(op2.op)(input.tail.head)
      JObject(List(JField(op1.key.name, res1), JField(op2.key.name, res2)))
    }


    def members: List[FieldDefinition[_]] = List(op1, op2)
  }


  /** Represents a required HList with three properties A,B,C */
  final case class HList3[A, B, C](
                                    op1: FieldDefinition[A],
                                    op2: FieldDefinition[B],
                                    op3: FieldDefinition[C]
                                  )
    extends ToHListDataDefinitionOp[A :: B :: C :: HNil] {


    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      (jsonProducer: JsonProducer) => {
        (r1(jsonProducer.child(op1.key)), r2(jsonProducer.child(op2.key)), r3(jsonProducer.child(op3.key)))
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
                                       op4: FieldDefinition[D])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: HNil] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      (jsonProducer: JsonProducer) => {
        (r1(jsonProducer.child(op1.key)),
          r2(jsonProducer.child(op2.key)),
          r3(jsonProducer.child(op3.key)),
          r4(jsonProducer.child(op4.key))).mapN(_ :: _ :: _ :: _ :: HNil)
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
                                          op5: FieldDefinition[E])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: HNil] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      (jsonProducer: JsonProducer) => {
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: HNil)
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
  }


  /** Represents a required HList with six properties. */
  final case class HList6[A, B, C, D, E, F](
                                             op1: FieldDefinition[A],
                                             op2: FieldDefinition[B],
                                             op3: FieldDefinition[C],
                                             op4: FieldDefinition[D],
                                             op5: FieldDefinition[E],
                                             op6: FieldDefinition[F])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: HNil] {

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: HNil] = {
      val r1 = f(op1.op)
      val r2 = f(op2.op)
      val r3 = f(op3.op)
      val r4 = f(op4.op)
      val r5 = f(op5.op)
      val r6 = f(op6.op)
      (jsonProducer: JsonProducer) => {
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer),
          r6(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: HNil)
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
                                                op7: FieldDefinition[G])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: HNil] {

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
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer),
          r6(jsonProducer),
          r7(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
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
  }


  /** Represents a required HList with six properties. */
  final case class HList8[A, B, C, D, E, F, G, H](
                                                   op1: FieldDefinition[A],
                                                   op2: FieldDefinition[B],
                                                   op3: FieldDefinition[C],
                                                   op4: FieldDefinition[D],
                                                   op5: FieldDefinition[E],
                                                   op6: FieldDefinition[F],
                                                   op7: FieldDefinition[G],
                                                   op8: FieldDefinition[H])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: HNil] {

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
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer),
          r6(jsonProducer),
          r7(jsonProducer),
          r8(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
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
                                                      op9: FieldDefinition[I])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] {

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
        (r1(jsonProducer.child(op1.key)),
          r2(jsonProducer.child(op2.key)),
          r3(jsonProducer.child(op3.key)),
          r4(jsonProducer.child(op4.key)),
          r5(jsonProducer.child(op5.key)),
          r6(jsonProducer.child(op6.key)),
          r7(jsonProducer.child(op7.key)),
          r8(jsonProducer.child(op8.key)),
          r9(jsonProducer.child(op9.key))).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
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
                                                          op10: FieldDefinition[J])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] {

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
        (r1(jsonProducer.child(op1.key)),
          r2(jsonProducer.child(op2.key)),
          r3(jsonProducer.child(op3.key)),
          r4(jsonProducer.child(op4.key)),
          r5(jsonProducer.child(op5.key)),
          r6(jsonProducer.child(op6.key)),
          r7(jsonProducer.child(op7.key)),
          r8(jsonProducer.child(op8.key)),
          r9(jsonProducer.child(op9.key)),
          r10(jsonProducer.child(op10.key))).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
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
                                                             op11: FieldDefinition[K])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] {

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
        (r1(jsonProducer.child(op1.key)),
          r2(jsonProducer.child(op2.key)),
          r3(jsonProducer.child(op3.key)),
          r4(jsonProducer.child(op4.key)),
          r5(jsonProducer.child(op5.key)),
          r6(jsonProducer.child(op6.key)),
          r7(jsonProducer.child(op7.key)),
          r8(jsonProducer.child(op8.key)),
          r9(jsonProducer.child(op9.key)),
          r10(jsonProducer.child(op10.key)),
          r11(jsonProducer.child(op11.key))).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
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
                                                                op12: FieldDefinition[L])
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] {

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
        (r1(jsonProducer.child(op1.key)),
          r2(jsonProducer.child(op2.key)),
          r3(jsonProducer.child(op3.key)),
          r4(jsonProducer.child(op4.key)),
          r5(jsonProducer.child(op5.key)),
          r6(jsonProducer.child(op6.key)),
          r7(jsonProducer.child(op7.key)),
          r8(jsonProducer.child(op8.key)),
          r9(jsonProducer.child(op9.key)),
          r10(jsonProducer.child(op10.key)),
          r11(jsonProducer.child(op11.key)),
          r12(jsonProducer.child(op12.key))).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
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

}
