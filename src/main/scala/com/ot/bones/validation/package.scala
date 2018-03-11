package com.ot.bones

import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.interpreter.EncoderInterpreter.ValidateAndEncode
import com.ot.bones.interpreter.ExtractionInterpreter.{BoolProducer, ExtractionError, ExtractionErrors, JsonProducer, RequiredObjectError, StringProducer, ValidateFromProducer, ValidationError, ValidationResultNel}
import com.ot.bones.validation.ValidationDefinition.{ToOptionalValidation, ValidationOp}
import com.ot.bones.validation._
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import shapeless.{::, HList, HNil}

package object validation {

  def validateOption[A](a: Option[A], validations: Seq[ValidationOp[A]]): Validated[NonEmptyList[ValidationError[A]], Option[A]] = a match {
    case Some(v) =>
      ValidationUtil.runAndMapValidations(v, validations).map(Some(_))
    case None =>
      Valid(None)
  }

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition.. */
  trait DataDefinitionOp[A] {

    //lift any DataDefinition into a FreeApplicative
    def lift: DataDefinition[A] = FreeApplicative.lift(this)

    def extract(producer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidationResultNel[A]
  }

  case class OptionalDataDefinition[B](dataDefinitionOp: DataDefinitionOp[B]) extends DataDefinitionOp[Option[B]] {
    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[Option[B]] = {
      dataDefinitionOp.extract(jsonProducer, functionK) match {
        case Valid(v) => Valid(Some(v))
        case Invalid(x) => {
          x.filterNot(_.isInstanceOf[RequiredObjectError]).toNel match {
            case Some(nel) => Invalid(nel)
            case None => Valid(None)
          }
        }
      }
    }
  }

  trait ToOptionalData[A] extends DataDefinitionOp[A] {
    def toOption = OptionalDataDefinition(this)
  }

  type DataDefinition[A] = FreeApplicative[DataDefinitionOp, A]

}

object BooleanDataDefinition {

  final case class BooleanData() extends DataDefinitionOp[Boolean] with ToOptionalData[Boolean] {

    def extract(producer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, Boolean] = {
      producer.produceBool.leftMap(NonEmptyList.one) andThen {
        case Some(x) => Valid(x)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }

  }

}

/** Convert from base Json type to a more specific type, such as converting a String to a UUID */
//final case class DataConversion[A, D <: DataDefinitionOp[A], C[_], O](d: D, o: C[O])

object EitherDataDefinition {

  final case class EitherData[A, B](definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B])
    extends DataDefinitionOp[Either[A, B]] with ToOptionalData[Either[A, B]] {

    def extract(producer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, Either[A, B]] = {
      functionK.apply(definitionA).apply(producer).map(Left(_))
        .orElse(functionK.apply(definitionB).apply(producer).map(Right(_)))
    }
  }

}

object IntDataDefinition {


  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    */
  final case class IntData() extends DataDefinitionOp[Int] with ToOptionalData[Int] {

    def extract(producer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, Int] = {
      producer.produceInt.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }

  }

}

object ListValidation {

  final case class ListData[T](tDefinition: DataDefinitionOp[T]) extends DataDefinitionOp[List[T]] with ToOptionalData[List[T]] {

    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidationResultNel[List[T]] = {
      jsonProducer.produceList.leftMap(NonEmptyList.one).andThen {
        case Some(list) =>
          list.map(producer => {
            functionK(tDefinition)(producer)
          }).sequence[ValidationResultNel, T]
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }

    }

  }

}

object StringDataDefinition {

  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    */
  final case class StringData() extends DataDefinitionOp[String] with ToOptionalData[String] {

    def extract(producer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, String] = {
      producer.produceString.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }
  }


}


object ToHList {

  /** Used to create a generic extract method so we can extract values from the products. */
  abstract class ToHListDataDefinitionOp[L <: HList] extends DataDefinitionOp[L] with ToOptionalData[L] {

    /** Get a list of untyped members */
    def members: List[FieldDefinition[_, _]]

    /** Extract the child or children.  AA should be a Tuple */
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]

    def encodeMembers(value: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): Validated[ExtractionErrors, L] = {
      jsonProducer.produceObject.leftMap(NonEmptyList.one).andThen {
        case Some(producer) => extractMembers(functionK)(producer)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }

    def encode(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): L => ValidationResultNel[JValue] = {
      (l: L) => {
        encodeMembers(functionK).apply(l)
      }
    }

  }

  //  /** Used to create a generic extract method if we can extract optional values from the products. */
  //  abstract class ToOptionalHListDataDefinitionOp[L <: HList] extends DataDefinitionOp[Option[L]] {
  //
  //    /** The key used to extract the value from the JsonProducer */
  //    def members: List[FieldDefinition[_,_]]
  //
  //    /** Extract the child or children.  AA should be a Tuple */
  //    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]
  //
  //    /** This will be used to implement the FieldGroupOp in the context of the children. */
  //    def extract(key: Key, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer])
  //    : ValidateFromProducer[Option[L]] = {
  //      (json: JsonProducer) =>
  //      {
  //        json.child(key).produceObject.leftMap(NonEmptyList.one).andThen {
  //          case Some(producer) =>
  //            extractMembers(functionK)(producer).map(Some(_))
  //          case None => Valid(None)
  //        }
  //      }
  //    }
  //
  //    def extractRoot(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[Option[L]] = {
  //      (json: JsonProducer) =>
  //      {
  //        json.produceObject.leftMap(NonEmptyList.one).andThen {
  //          case Some(producer) =>
  //            extractMembers(functionK)(producer).map(Some(_))
  //          case None => Valid(None)
  //        }
  //      }
  //
  //    }
  //  }


  /** Represents a required HList with two properties A and B */
  final case class HList2[A, B](
                                 op1: FieldDefinition[A, DataDefinitionOp[A]],
                                 op2: FieldDefinition[B, DataDefinitionOp[B]]
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      (input: (A :: B :: HNil)) => {
        (res1(input.head), res2(input.tail.head)).mapN((j1, j2) => JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2))))
      }
    }


    def members: List[FieldDefinition[_, _]] = List(op1, op2)
  }


  /** Represents a required HList with three properties A,B,C */
  final case class HList3[A, B, C](
                                    op1: FieldDefinition[A, DataDefinitionOp[A]],
                                    op2: FieldDefinition[B, DataDefinitionOp[B]],
                                    op3: FieldDefinition[C, DataDefinitionOp[C]]
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      (input: (A :: B :: C :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head)).mapN((j1, j2, j3) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3)))
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3)

  }

  /** Represents a required HList with four properties A,B,C,D */
  final case class HList4[A, B, C, D](
                                       op1: FieldDefinition[A, DataDefinitionOp[A]],
                                       op2: FieldDefinition[B, DataDefinitionOp[B]],
                                       op3: FieldDefinition[C, DataDefinitionOp[C]],
                                       op4: FieldDefinition[D, DataDefinitionOp[D]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      (input: (A :: B :: C :: D :: HNil)) => {
        (res1(input.head), res2(input.tail.head),
          res3(input.tail.tail.head), res4(input.tail.tail.tail.head)).mapN((j1, j2, j3, j4) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4)))
        )
      }
    }


    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4)
  }


  /** Represents a required HList with four properties. */
  final case class HList5[A, B, C, D, E](
                                          op1: FieldDefinition[A, DataDefinitionOp[A]],
                                          op2: FieldDefinition[B, DataDefinitionOp[B]],
                                          op3: FieldDefinition[C, DataDefinitionOp[C]],
                                          op4: FieldDefinition[D, DataDefinitionOp[D]],
                                          op5: FieldDefinition[E, DataDefinitionOp[E]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      (input: (A :: B :: C :: D :: E :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head)).mapN((j1, j2, j3, j4, j5) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5)))
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op4)
  }


  /** Represents a required HList with six properties. */
  final case class HList6[A, B, C, D, E, F](
                                             op1: FieldDefinition[A, DataDefinitionOp[A]],
                                             op2: FieldDefinition[B, DataDefinitionOp[B]],
                                             op3: FieldDefinition[C, DataDefinitionOp[C]],
                                             op4: FieldDefinition[D, DataDefinitionOp[D]],
                                             op5: FieldDefinition[E, DataDefinitionOp[E]],
                                             op6: FieldDefinition[F, DataDefinitionOp[F]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      (input: (A :: B :: C :: D :: E :: F :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head)).mapN((j1, j2, j3, j4, j5, j6) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6)))
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6)

  }


  /** Represents a required HList with six properties. */
  final case class HList7[A, B, C, D, E, F, G](
                                                op1: FieldDefinition[A, DataDefinitionOp[A]],
                                                op2: FieldDefinition[B, DataDefinitionOp[B]],
                                                op3: FieldDefinition[C, DataDefinitionOp[C]],
                                                op4: FieldDefinition[D, DataDefinitionOp[D]],
                                                op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                op6: FieldDefinition[F, DataDefinitionOp[F]],
                                                op7: FieldDefinition[G, DataDefinitionOp[G]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head)).mapN((j1, j2, j3, j4, j5, j6, j7) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6), JField(op7.key.name, j7)))
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6, op7)
  }


  /** Represents a required HList with six properties. */
  final case class HList8[A, B, C, D, E, F, G, H](
                                                   op1: FieldDefinition[A, DataDefinitionOp[A]],
                                                   op2: FieldDefinition[B, DataDefinitionOp[B]],
                                                   op3: FieldDefinition[C, DataDefinitionOp[C]],
                                                   op4: FieldDefinition[D, DataDefinitionOp[D]],
                                                   op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                   op6: FieldDefinition[F, DataDefinitionOp[F]],
                                                   op7: FieldDefinition[G, DataDefinitionOp[G]],
                                                   op8: FieldDefinition[H, DataDefinitionOp[H]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: HNil] = {
      val res1 = functionK(op1.op)
      val res2 = functionK(op2.op)
      val res3 = functionK(op3.op)
      val res4 = functionK(op4.op)
      val res5 = functionK(op5.op)
      val res6 = functionK(op6.op)
      val res7 = functionK(op7.op)
      val res8 = functionK(op8.op)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN((j1, j2, j3, j4, j5, j6, j7, j8) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6), JField(op7.key.name, j7),
            JField(op8.key.name, j8))
          )
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6, op7, op8)

  }

  /** Represents a required HList with six properties. */
  final case class HList9[A, B, C, D, E, F, G, H, I](
                                                      op1: FieldDefinition[A, DataDefinitionOp[A]],
                                                      op2: FieldDefinition[B, DataDefinitionOp[B]],
                                                      op3: FieldDefinition[C, DataDefinitionOp[C]],
                                                      op4: FieldDefinition[D, DataDefinitionOp[D]],
                                                      op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                      op6: FieldDefinition[F, DataDefinitionOp[F]],
                                                      op7: FieldDefinition[G, DataDefinitionOp[G]],
                                                      op8: FieldDefinition[H, DataDefinitionOp[H]],
                                                      op9: FieldDefinition[I, DataDefinitionOp[I]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] = {
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
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN((j1, j2, j3, j4, j5, j6, j7, j8, j9) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6), JField(op7.key.name, j7),
            JField(op8.key.name, j8), JField(op9.key.name, j9))
          )
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9)
  }

  /** Represents a required HList with six properties. */
  final case class HList10[A, B, C, D, E, F, G, H, I, J](
                                                          op1: FieldDefinition[A, DataDefinitionOp[A]],
                                                          op2: FieldDefinition[B, DataDefinitionOp[B]],
                                                          op3: FieldDefinition[C, DataDefinitionOp[C]],
                                                          op4: FieldDefinition[D, DataDefinitionOp[D]],
                                                          op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                          op6: FieldDefinition[F, DataDefinitionOp[F]],
                                                          op7: FieldDefinition[G, DataDefinitionOp[G]],
                                                          op8: FieldDefinition[H, DataDefinitionOp[H]],
                                                          op9: FieldDefinition[I, DataDefinitionOp[I]],
                                                          op10: FieldDefinition[J, DataDefinitionOp[J]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] = {
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
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN((j1, j2, j3, j4, j5, j6, j7, j8, j9, j10) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6), JField(op7.key.name, j7),
            JField(op8.key.name, j8), JField(op9.key.name, j9), JField(op10.key.name, j10))
          )
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)
  }


  /** Represents a required HList with six properties. */
  final case class HList11[A, B, C, D, E, F, G, H, I, J, K](
                                                             op1: FieldDefinition[A, DataDefinitionOp[A]],
                                                             op2: FieldDefinition[B, DataDefinitionOp[B]],
                                                             op3: FieldDefinition[C, DataDefinitionOp[C]],
                                                             op4: FieldDefinition[D, DataDefinitionOp[D]],
                                                             op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                             op6: FieldDefinition[F, DataDefinitionOp[F]],
                                                             op7: FieldDefinition[G, DataDefinitionOp[G]],
                                                             op8: FieldDefinition[H, DataDefinitionOp[H]],
                                                             op9: FieldDefinition[I, DataDefinitionOp[I]],
                                                             op10: FieldDefinition[J, DataDefinitionOp[J]],
                                                             op11: FieldDefinition[K, DataDefinitionOp[K]])
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

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] = {
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
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res11(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN((j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6), JField(op7.key.name, j7),
            JField(op8.key.name, j8), JField(op9.key.name, j9), JField(op10.key.name, j10), JField(op11.key.name, j11))
          )
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)
  }


  /** Represents a required HList with six properties. */
  final case class HList12[A, B, C, D, E, F, G, H, I, J, K, L](
                                                                op1: FieldDefinition[A, DataDefinitionOp[A]],
                                                                op2: FieldDefinition[B, DataDefinitionOp[B]],
                                                                op3: FieldDefinition[C, DataDefinitionOp[C]],
                                                                op4: FieldDefinition[D, DataDefinitionOp[D]],
                                                                op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                                op6: FieldDefinition[F, DataDefinitionOp[F]],
                                                                op7: FieldDefinition[G, DataDefinitionOp[G]],
                                                                op8: FieldDefinition[H, DataDefinitionOp[H]],
                                                                op9: FieldDefinition[I, DataDefinitionOp[I]],
                                                                op10: FieldDefinition[J, DataDefinitionOp[J]],
                                                                op11: FieldDefinition[K, DataDefinitionOp[K]],
                                                                op12: FieldDefinition[L, DataDefinitionOp[L]])
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
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer),
          r6(jsonProducer),
          r7(jsonProducer),
          r8(jsonProducer),
          r9(jsonProducer),
          r10(jsonProducer),
          r11(jsonProducer),
          r12(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] = {
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
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res11(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res12(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN((j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12) =>
          JObject(List(JField(op1.key.name, j1), JField(op2.key.name, j2), JField(op3.key.name, j3),
            JField(op4.key.name, j4), JField(op5.key.name, j5), JField(op6.key.name, j6), JField(op7.key.name, j7),
            JField(op8.key.name, j8), JField(op9.key.name, j9), JField(op10.key.name, j10), JField(op11.key.name, j11), JField(op12.key.name, j12))
          )
        )
      }
    }

    def members: List[FieldDefinition[_, _]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)
  }

}


