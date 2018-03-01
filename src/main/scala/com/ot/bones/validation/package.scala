package com.ot.bones

import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.free.FreeApplicative
import cats.implicits._
import com.ot.bones.interpreter.EncoderInterpreter.ValidateAndEncode
import com.ot.bones.interpreter.ExtractionInterpreter.{BoolProducer, ExtractionError, ExtractionErrors, JsonProducer, RequiredObjectError, StringProducer, ValidateFromProducer, ValidationResultNel}
import com.ot.bones.validation.ValidationDefinition.ValidationOp
import com.ot.bones.validation._
import net.liftweb.json.JsonAST.{JField, JObject, JValue}
import shapeless.{::, HList, HNil}

package object validation {

  /** DataDefinitionOp is the base class defining the FreeAp for each data definition..*/
  sealed trait DataDefinitionOp[A] {
    //lift any DataDefinition into a FreeApplicative
    def lift: DataDefinition[A] = FreeApplicative.lift(this)
  }

  type DataDefinition[A] = FreeApplicative[DataDefinitionOp, A]

}

object BooleanDataDefinition {

  final case class RequiredBoolean() extends DataDefinitionOp[Boolean] {

    def extract(producer: BoolProducer): Validated[ExtractionErrors, Boolean] = {
      producer.produceBool.leftMap(NonEmptyList.one) andThen {
        case Some(x) => Valid(x)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }
    }

    def optional: OptionalBoolean = OptionalBoolean()
  }

  final case class OptionalBoolean() extends DataDefinitionOp[Option[Boolean]] {

    def extract(producer: BoolProducer) : Validated[ExtractionErrors, Option[Boolean]] = {
      producer.produceBool.leftMap(NonEmptyList.one)
    }
  }

}

/** Convert from base Json type to a more specific type, such as converting a String to a UUID */
final case class DataConversion[A, D <: DataDefinitionOp[A], C[_], O](d: D, o: C[O])

object EitherDataDefinition {

  final case class RequiredEither[A,B](definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B])
    extends DataDefinitionOp[Either[A,B]] {

    def extract(producer:JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[Either[A,B]] = {
      functionK.apply(definitionA).apply(producer).map(Left(_))
        .orElse(functionK.apply(definitionB).apply(producer).map(Right(_)))
    }
  }

  final case class OptionalEither[A,B](definitionA: DataDefinitionOp[A], definitionB: DataDefinitionOp[B])
    extends DataDefinitionOp[Option[Either[A,B]]] {

    def extract(producer:JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[Option[Either[A,B]]] = {
      functionK.apply(definitionA).apply(producer).map(Left(_))
        .orElse(functionK.apply(definitionB).apply(producer).map(Right(_)))
      match {
        case Invalid(NonEmptyList(RequiredObjectError(), _)) => Valid(None)
        case Valid(x) => Valid(Some(x))
        case Invalid(x) => Invalid(x)
      }
    }
  }

}

object IntDataDefinition {




  /**
    * FieldGroup Operation declares that a key is an optional string and passes the specified list of validation.
    * @param validations List of validations that the String must pass.
    */

  final case class OptionalInt(validations: List[ValidationOp[Int]]) extends DataDefinitionOp[Option[Int]] {

    def extract(input: JsonProducer): Validated[ExtractionErrors, Option[Int]] = {
      input.produceInt.toValidatedNel.andThen((i: Option[Int]) => i match {
        case Some(int) =>
          ValidationUtil.runAndMapValidations(int, validations).map(Some(_))
        case None => Valid(None)
      })
    }


    def append(sv: ValidationOp[Int]): OptionalInt = OptionalInt(sv :: validations)
  }

  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    * @param validations List of validations that the String must pass.
    */
  final case class RequiredInt(validations: List[ValidationOp[Int]]) extends DataDefinitionOp[Int] {

    def optional(): OptionalInt = OptionalInt(validations)

    def extract(producer: JsonProducer): Validated[NonEmptyList[ExtractionError], Int] = {
      producer.produceInt.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }.andThen(i =>
        ValidationUtil.runAndMapValidations(i, validations)
      )
    }

    def append(sv: ValidationOp[Int]): RequiredInt = RequiredInt(sv :: validations)

  }

}

object ListValidation {

  final case class RequiredList[T](tDefinition: DataDefinitionOp[T]) extends DataDefinitionOp[List[T]] {

    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[List[T]] = {
      jsonProducer.produceList.leftMap(NonEmptyList.one).andThen {
        case Some(list) => {
          list.map(producer => {
            functionK(tDefinition)(producer)
          }).sequence[ValidationResultNel,T]
        }
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }

    }

    def optional() = OptionalList(tDefinition)

  }

  final case class OptionalList[T](tDefinition: DataDefinitionOp[T]) extends DataDefinitionOp[Option[List[T]]] {

    def extract(jsonProducer: JsonProducer, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]) : ValidationResultNel[Option[List[T]]] = {
      jsonProducer.produceList.leftMap(NonEmptyList.one).andThen {
        case Some(list) => {
          list.map(producer => {
            functionK(tDefinition)(producer)
          }).sequence[ValidationResultNel,T].map(Some(_))
        }
        case None => Valid(None)
      }

    }


  }



}

object StringDataDefinition {

  /**
    * FieldGroup Operation declares that a key is an optional string and passes the specified list of validation.
    * @param validations List of validations that the String must pass.
    */

  final case class OptionalString(validations: List[ValidationOp[String]]) extends DataDefinitionOp[Option[String]] {

    def extract(input: StringProducer): Validated[ExtractionErrors, Option[String]] = {
      input.produceString.toValidatedNel.andThen((s: Option[String]) => s match {
        case Some(str) =>
          ValidationUtil.runAndMapValidations(str, validations).map(Some(_))
        case None => Valid(None)
      })
    }
  }

  /**
    * FieldGroup Operation declares that a key is a required string and passes the specified list of validation.
    * @param validations List of validations that the String must pass.
    */
  final case class RequiredString(validations: List[ValidationOp[String]]) extends DataDefinitionOp[String] {

    def is(f: RequiredString => RequiredString): RequiredString = f(this)

    def optional(): OptionalString = OptionalString(validations)

    def extract(producer: StringProducer): ValidationResultNel[String] = {
      producer.produceString.leftMap(NonEmptyList.one).andThen {
        case Some(e) => Valid(e)
        case None => Invalid(NonEmptyList.one(RequiredObjectError()))
      }.andThen(str =>
        ValidationUtil.runAndMapValidations(str, validations)
      )
    }

  }

}


object ToHList {

  /** Used to create a generic extract method so we can extract values from the products. */
  abstract class ToHListDataDefinitionOp[L <: HList] extends DataDefinitionOp[L] {

    /** Get a list of untyped members */
    def members: List[(Key, DataDefinitionOp[_])]

    /** Extract the child or children.  AA should be a Tuple */
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]

    def encodeMembers(value: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L] = {
      (json: JsonProducer) =>
      {
        json.produceObject.leftMap(NonEmptyList.one).andThen {
          case Some(producer) => extractMembers(functionK)(producer)
          case None           => Invalid(NonEmptyList.one(RequiredObjectError()))
        }
      }
    }

    def encode(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): L => ValidationResultNel[JValue] = {
      (l: L) => {
        encodeMembers(functionK).apply(l)
      }
    }

  }

  /** Used to create a generic extract method if we can extract optional values from the products. */
  abstract class ToOptionalHListDataDefinitionOp[L <: HList] extends DataDefinitionOp[Option[L]] {

    /** The key used to extract the value from the JsonProducer */
    def members: List[(Key, DataDefinitionOp[_])]

    /** Extract the child or children.  AA should be a Tuple */
    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(key: Key, functionK: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[Option[L]] = {
      (json: JsonProducer) =>
      {
        json.produceObject.leftMap(NonEmptyList.one).andThen {
          case Some(producer) =>
            extractMembers(functionK)(producer).map(Some(_))
          case None => Valid(None)
        }
      }
    }

    def extractRoot(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer]): ValidateFromProducer[Option[L]] = {
      (json: JsonProducer) =>
      {
        json.produceObject.leftMap(NonEmptyList.one).andThen {
          case Some(producer) =>
            extractMembers(functionK)(producer).map(Some(_))
          case None => Valid(None)
        }
      }

    }
  }

  /** Base trait for HList2 */
  trait ExtractHList2[A, B] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])

    def extractMembers(functionK: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      (jsonProducer: JsonProducer) =>
      {
        (res1(jsonProducer.resolve(op1._1)), res2(jsonProducer.resolve(op2._1)))
          .mapN(_ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      (input: (A :: B ::HNil)) => {
        (res1(input.head), res2(input.tail.head)).mapN( (j1, j2) => JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2))))
      }
    }


    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2)

  }

  /** Represents a required HList with two properties A and B */
  final case class HList2[A, B](op1: (Key, DataDefinitionOp[A]), op2: (Key, DataDefinitionOp[B]))
    extends ToHListDataDefinitionOp[A :: B :: HNil]
      with ExtractHList2[A, B] {

    def optional() = ToOptionalHList2(op1, op2)

  }

  /** Represents an optional object with two properties A and B */
  final case class ToOptionalHList2[A, B](op1: (Key, DataDefinitionOp[A]), op2: (Key, DataDefinitionOp[B]))
    extends ToOptionalHListDataDefinitionOp[A :: B :: HNil]
      with ExtractHList2[A, B] {}

  /** Base trait contains common functionality for extracting an HList with 3 elements */
  trait ExtractHList3[A, B, C] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      (jsonProducer: JsonProducer) =>
      {
        (r1(jsonProducer.resolve(op1._1)), r2(jsonProducer.resolve(op2._1)), r3(jsonProducer.resolve(op3._1)))
          .mapN(_ :: _ :: _ :: HNil)
      }

    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C:: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      (input: (A :: B :: C ::HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head)).mapN( (j1, j2, j3) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3)))
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3)

  }

  /** Represents a required HList with three properties A,B,C */
  final case class HList3[A, B, C](
                                    op1: (Key, DataDefinitionOp[A]),
                                    op2: (Key, DataDefinitionOp[B]),
                                    op3: (Key, DataDefinitionOp[C]))
    extends ToHListDataDefinitionOp[A :: B :: C :: HNil]
      with ExtractHList3[A, B, C] {

    def optional() = OptionalHList3(op1, op2, op3)

  }

  /** Represents an optional HList with three properties A,B,C */
  final case class OptionalHList3[A, B, C](
                                            op1: (Key, DataDefinitionOp[A]),
                                            op2: (Key, DataDefinitionOp[B]),
                                            op3: (Key, DataDefinitionOp[C]))
    extends ToHListDataDefinitionOp[A :: B :: C :: HNil]
      with ExtractHList3[A, B, C] {}

  /** Base trait contains common functionality for extracting an HList with 4 elements */
  trait ExtractHList4[A, B, C, D] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      (jsonProducer: JsonProducer) =>
      {
        (r1(jsonProducer.resolve(op1._1)),
          r2(jsonProducer.resolve(op2._1)),
          r3(jsonProducer.resolve(op3._1)),
          r4(jsonProducer.resolve(op4._1))).mapN(_ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      (input: (A :: B :: C :: D :: HNil)) => {
        (res1(input.head), res2(input.tail.head),
          res3(input.tail.tail.head), res4(input.tail.tail.tail.head)).mapN( (j1, j2, j3, j4) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4)))
        )
      }
    }


    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4)

  }

  /** Represents a required HList with four properties A,B,C,D */
  final case class HList4[A, B, C, D](
                                       op1: (Key, DataDefinitionOp[A]),
                                       op2: (Key, DataDefinitionOp[B]),
                                       op3: (Key, DataDefinitionOp[C]),
                                       op4: (Key, DataDefinitionOp[D]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: HNil]
      with ExtractHList4[A, B, C, D] {

    def optional() = OptionalHList4(op1, op2, op3, op4)

  }

  /** Represents an optional HList with four properties A,B,C,D */
  final case class OptionalHList4[A, B, C, D](
                                               op1: (Key, DataDefinitionOp[A]),
                                               op2: (Key, DataDefinitionOp[B]),
                                               op3: (Key, DataDefinitionOp[C]),
                                               op4: (Key, DataDefinitionOp[D]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: HNil]
      with ExtractHList4[A, B, C, D] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList5[A, B, C, D, E] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      (jsonProducer: JsonProducer) =>
      {
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      (input: (A :: B :: C :: D :: E :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head)).mapN( (j1, j2, j3, j4, j5) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5)))
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op4)

  }

  /** Represents a required HList with four properties. */
  final case class HList5[A, B, C, D, E](
                                          op1: (Key, DataDefinitionOp[A]),
                                          op2: (Key, DataDefinitionOp[B]),
                                          op3: (Key, DataDefinitionOp[C]),
                                          op4: (Key, DataDefinitionOp[D]),
                                          op5: (Key, DataDefinitionOp[E]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: HNil]
      with ExtractHList5[A, B, C, D, E] {

    def optional() = OptionalHList5(op1, op2, op3, op4, op5)

  }

  /** Represents an optional HList with five properties. */
  final case class OptionalHList5[A, B, C, D, E](
                                                  op1: (Key, DataDefinitionOp[A]),
                                                  op2: (Key, DataDefinitionOp[B]),
                                                  op3: (Key, DataDefinitionOp[C]),
                                                  op4: (Key, DataDefinitionOp[D]),
                                                  op5: (Key, DataDefinitionOp[E]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: HNil]
      with ExtractHList5[A, B, C, D, E] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList6[A, B, C, D, E, F] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      (jsonProducer: JsonProducer) =>
      {
        (r1(jsonProducer),
          r2(jsonProducer),
          r3(jsonProducer),
          r4(jsonProducer),
          r5(jsonProducer),
          r6(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      (input: (A :: B :: C :: D :: E :: F :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head)).mapN( (j1, j2, j3, j4, j5, j6) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6)))
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6)

  }

  /** Represents a required HList with six properties. */
  final case class HList6[A, B, C, D, E, F](
                                             op1: (Key, DataDefinitionOp[A]),
                                             op2: (Key, DataDefinitionOp[B]),
                                             op3: (Key, DataDefinitionOp[C]),
                                             op4: (Key, DataDefinitionOp[D]),
                                             op5: (Key, DataDefinitionOp[E]),
                                             op6: (Key, DataDefinitionOp[F]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: HNil]
      with ExtractHList6[A, B, C, D, E, F] {

    def optional() = OptionalHList6(op1, op2, op3, op4, op5, op6)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList6[A, B, C, D, E, F](
                                                     op1: (Key, DataDefinitionOp[A]),
                                                     op2: (Key, DataDefinitionOp[B]),
                                                     op3: (Key, DataDefinitionOp[C]),
                                                     op4: (Key, DataDefinitionOp[D]),
                                                     op5: (Key, DataDefinitionOp[E]),
                                                     op6: (Key, DataDefinitionOp[F]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: HNil]
      with ExtractHList6[A, B, C, D, E, F] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList7[A, B, C, D, E, F, G] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])
    val op7: (Key, DataDefinitionOp[G])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      val r7 = f(op7._2)
      (jsonProducer: JsonProducer) =>
      {
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
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      val res7 = functionK(op7._2)
      (input: (A :: B :: C :: D :: E :: F :: G :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head)).mapN( (j1, j2, j3, j4, j5, j6, j7) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6), JField(op7._1.name, j7)))
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6, op7)

  }

  /** Represents a required HList with six properties. */
  final case class HList7[A, B, C, D, E, F, G](
                                                op1: (Key, DataDefinitionOp[A]),
                                                op2: (Key, DataDefinitionOp[B]),
                                                op3: (Key, DataDefinitionOp[C]),
                                                op4: (Key, DataDefinitionOp[D]),
                                                op5: (Key, DataDefinitionOp[E]),
                                                op6: (Key, DataDefinitionOp[F]),
                                                op7: (Key, DataDefinitionOp[G]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: HNil]
      with ExtractHList7[A, B, C, D, E, F, G] {

    def optional() = OptionalHList7(op1, op2, op3, op4, op5, op6, op7)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList7[A, B, C, D, E, F, G](
                                                        op1: (Key, DataDefinitionOp[A]),
                                                        op2: (Key, DataDefinitionOp[B]),
                                                        op3: (Key, DataDefinitionOp[C]),
                                                        op4: (Key, DataDefinitionOp[D]),
                                                        op5: (Key, DataDefinitionOp[E]),
                                                        op6: (Key, DataDefinitionOp[F]),
                                                        op7: (Key, DataDefinitionOp[G]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: HNil]
      with ExtractHList7[A, B, C, D, E, F, G] {}


  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList8[A, B, C, D, E, F, G, H] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])
    val op7: (Key, DataDefinitionOp[G])
    val op8: (Key, DataDefinitionOp[H])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      val r7 = f(op7._2)
      val r8 = f(op8._2)
      (jsonProducer: JsonProducer) =>
      {
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
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      val res7 = functionK(op7._2)
      val res8 = functionK(op8._2)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN( (j1, j2, j3, j4, j5, j6, j7, j8) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6), JField(op7._1.name, j7),
            JField(op8._1.name, j8))
          )
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6, op7, op8)

  }

  /** Represents a required HList with six properties. */
  final case class HList8[A, B, C, D, E, F, G, H](
                                                   op1: (Key, DataDefinitionOp[A]),
                                                   op2: (Key, DataDefinitionOp[B]),
                                                   op3: (Key, DataDefinitionOp[C]),
                                                   op4: (Key, DataDefinitionOp[D]),
                                                   op5: (Key, DataDefinitionOp[E]),
                                                   op6: (Key, DataDefinitionOp[F]),
                                                   op7: (Key, DataDefinitionOp[G]),
                                                   op8: (Key, DataDefinitionOp[H]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: HNil]
      with ExtractHList8[A, B, C, D, E, F, G, H] {

    def optional() = OptionalHList8(op1, op2, op3, op4, op5, op6, op7, op8)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList8[A, B, C, D, E, F, G, H](
                                                           op1: (Key, DataDefinitionOp[A]),
                                                           op2: (Key, DataDefinitionOp[B]),
                                                           op3: (Key, DataDefinitionOp[C]),
                                                           op4: (Key, DataDefinitionOp[D]),
                                                           op5: (Key, DataDefinitionOp[E]),
                                                           op6: (Key, DataDefinitionOp[F]),
                                                           op7: (Key, DataDefinitionOp[G]),
                                                           op8: (Key, DataDefinitionOp[H]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: HNil]
      with ExtractHList8[A, B, C, D, E, F, G, H] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList9[A, B, C, D, E, F, G, H, I] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])
    val op7: (Key, DataDefinitionOp[G])
    val op8: (Key, DataDefinitionOp[H])
    val op9: (Key, DataDefinitionOp[I])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      val r7 = f(op7._2)
      val r8 = f(op8._2)
      val r9 = f(op9._2)
      (jsonProducer: JsonProducer) =>
      {
        (r1(jsonProducer.resolve(op1._1)),
          r2(jsonProducer.resolve(op2._1)),
          r3(jsonProducer.resolve(op3._1)),
          r4(jsonProducer.resolve(op4._1)),
          r5(jsonProducer.resolve(op5._1)),
          r6(jsonProducer.resolve(op6._1)),
          r7(jsonProducer.resolve(op7._1)),
          r8(jsonProducer.resolve(op8._1)),
          r9(jsonProducer.resolve(op9._1))).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      val res7 = functionK(op7._2)
      val res8 = functionK(op8._2)
      val res9 = functionK(op9._2)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN( (j1, j2, j3, j4, j5, j6, j7, j8, j9) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6), JField(op7._1.name, j7),
            JField(op8._1.name, j8), JField(op9._1.name, j9))
          )
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9)

  }

  /** Represents a required HList with six properties. */
  final case class HList9[A, B, C, D, E, F, G, H, I](
                                                      op1: (Key, DataDefinitionOp[A]),
                                                      op2: (Key, DataDefinitionOp[B]),
                                                      op3: (Key, DataDefinitionOp[C]),
                                                      op4: (Key, DataDefinitionOp[D]),
                                                      op5: (Key, DataDefinitionOp[E]),
                                                      op6: (Key, DataDefinitionOp[F]),
                                                      op7: (Key, DataDefinitionOp[G]),
                                                      op8: (Key, DataDefinitionOp[H]),
                                                      op9: (Key, DataDefinitionOp[I]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil]
      with ExtractHList9[A, B, C, D, E, F, G, H, I] {

    def optional() = OptionalHList9(op1, op2, op3, op4, op5, op6, op7, op8, op9)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList9[A, B, C, D, E, F, G, H, I](
                                                              op1: (Key, DataDefinitionOp[A]),
                                                              op2: (Key, DataDefinitionOp[B]),
                                                              op3: (Key, DataDefinitionOp[C]),
                                                              op4: (Key, DataDefinitionOp[D]),
                                                              op5: (Key, DataDefinitionOp[E]),
                                                              op6: (Key, DataDefinitionOp[F]),
                                                              op7: (Key, DataDefinitionOp[G]),
                                                              op8: (Key, DataDefinitionOp[H]),
                                                              op9: (Key, DataDefinitionOp[I]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil]
      with ExtractHList9[A, B, C, D, E, F, G, H, I] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList10[A, B, C, D, E, F, G, H, I, J] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])
    val op7: (Key, DataDefinitionOp[G])
    val op8: (Key, DataDefinitionOp[H])
    val op9: (Key, DataDefinitionOp[I])
    val op10: (Key, DataDefinitionOp[J])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      val r7 = f(op7._2)
      val r8 = f(op8._2)
      val r9 = f(op9._2)
      val r10 = f(op10._2)
      (jsonProducer: JsonProducer) =>
      {
        (r1(jsonProducer.resolve(op1._1)),
          r2(jsonProducer.resolve(op2._1)),
          r3(jsonProducer.resolve(op3._1)),
          r4(jsonProducer.resolve(op4._1)),
          r5(jsonProducer.resolve(op5._1)),
          r6(jsonProducer.resolve(op6._1)),
          r7(jsonProducer.resolve(op7._1)),
          r8(jsonProducer.resolve(op8._1)),
          r9(jsonProducer.resolve(op9._1)),
          r10(jsonProducer.resolve(op10._1))).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      val res7 = functionK(op7._2)
      val res8 = functionK(op8._2)
      val res9 = functionK(op9._2)
      val res10 = functionK(op10._2)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN( (j1, j2, j3, j4, j5, j6, j7, j8, j9, j10) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6), JField(op7._1.name, j7),
            JField(op8._1.name, j8), JField(op9._1.name, j9), JField(op10._1.name, j10))
          )
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  }

  /** Represents a required HList with six properties. */
  final case class HList10[A, B, C, D, E, F, G, H, I, J](
                                                          op1: (Key, DataDefinitionOp[A]),
                                                          op2: (Key, DataDefinitionOp[B]),
                                                          op3: (Key, DataDefinitionOp[C]),
                                                          op4: (Key, DataDefinitionOp[D]),
                                                          op5: (Key, DataDefinitionOp[E]),
                                                          op6: (Key, DataDefinitionOp[F]),
                                                          op7: (Key, DataDefinitionOp[G]),
                                                          op8: (Key, DataDefinitionOp[H]),
                                                          op9: (Key, DataDefinitionOp[I]),
                                                          op10: (Key, DataDefinitionOp[J]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil]
      with ExtractHList10[A, B, C, D, E, F, G, H, I, J] {

    def optional() = OptionalHList10(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList10[A, B, C, D, E, F, G, H, I, J](
                                                                  op1: (Key, DataDefinitionOp[A]),
                                                                  op2: (Key, DataDefinitionOp[B]),
                                                                  op3: (Key, DataDefinitionOp[C]),
                                                                  op4: (Key, DataDefinitionOp[D]),
                                                                  op5: (Key, DataDefinitionOp[E]),
                                                                  op6: (Key, DataDefinitionOp[F]),
                                                                  op7: (Key, DataDefinitionOp[G]),
                                                                  op8: (Key, DataDefinitionOp[H]),
                                                                  op9: (Key, DataDefinitionOp[I]),
                                                                  op10: (Key, DataDefinitionOp[J]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil]
      with ExtractHList10[A, B, C, D, E, F, G, H, I, J] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList11[A, B, C, D, E, F, G, H, I, J, K] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])
    val op7: (Key, DataDefinitionOp[G])
    val op8: (Key, DataDefinitionOp[H])
    val op9: (Key, DataDefinitionOp[I])
    val op10: (Key, DataDefinitionOp[J])
    val op11: (Key, DataDefinitionOp[K])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      val r7 = f(op7._2)
      val r8 = f(op8._2)
      val r9 = f(op9._2)
      val r10 = f(op10._2)
      val r11 = f(op11._2)
      (jsonProducer: JsonProducer) =>
      {
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
          r11(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def encodeMembers(functionK: FunctionK[DataDefinitionOp, ValidateAndEncode]): ValidateAndEncode[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] = {
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      val res7 = functionK(op7._2)
      val res8 = functionK(op8._2)
      val res9 = functionK(op9._2)
      val res10 = functionK(op10._2)
      val res11 = functionK(op11._2)
      (input: (A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil)) => {
        (res1(input.head), res2(input.tail.head), res3(input.tail.tail.head),
          res4(input.tail.tail.tail.head), res5(input.tail.tail.tail.tail.head),
          res6(input.tail.tail.tail.tail.tail.head),
          res7(input.tail.tail.tail.tail.tail.tail.head),
          res8(input.tail.tail.tail.tail.tail.tail.tail.head),
          res9(input.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res10(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.head),
          res11(input.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
        ).mapN( (j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6), JField(op7._1.name, j7),
            JField(op8._1.name, j8), JField(op9._1.name, j9), JField(op10._1.name, j10), JField(op11._1.name, j11))
          )
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  }

  /** Represents a required HList with six properties. */
  final case class HList11[A, B, C, D, E, F, G, H, I, J, K](
                                                             op1: (Key, DataDefinitionOp[A]),
                                                             op2: (Key, DataDefinitionOp[B]),
                                                             op3: (Key, DataDefinitionOp[C]),
                                                             op4: (Key, DataDefinitionOp[D]),
                                                             op5: (Key, DataDefinitionOp[E]),
                                                             op6: (Key, DataDefinitionOp[F]),
                                                             op7: (Key, DataDefinitionOp[G]),
                                                             op8: (Key, DataDefinitionOp[H]),
                                                             op9: (Key, DataDefinitionOp[I]),
                                                             op10: (Key, DataDefinitionOp[J]),
                                                             op11: (Key, DataDefinitionOp[K]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil]
      with ExtractHList11[A, B, C, D, E, F, G, H, I, J, K] {

    def optional() = OptionalHList11(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList11[A, B, C, D, E, F, G, H, I, J,K](
                                                                    op1: (Key, DataDefinitionOp[A]),
                                                                    op2: (Key, DataDefinitionOp[B]),
                                                                    op3: (Key, DataDefinitionOp[C]),
                                                                    op4: (Key, DataDefinitionOp[D]),
                                                                    op5: (Key, DataDefinitionOp[E]),
                                                                    op6: (Key, DataDefinitionOp[F]),
                                                                    op7: (Key, DataDefinitionOp[G]),
                                                                    op8: (Key, DataDefinitionOp[H]),
                                                                    op9: (Key, DataDefinitionOp[I]),
                                                                    op10: (Key, DataDefinitionOp[J]),
                                                                    op11: (Key, DataDefinitionOp[K]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil]
      with ExtractHList11[A, B, C, D, E, F, G, H, I, J, K] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList12[A, B, C, D, E, F, G, H, I, J, K, L] {
    val op1: (Key, DataDefinitionOp[A])
    val op2: (Key, DataDefinitionOp[B])
    val op3: (Key, DataDefinitionOp[C])
    val op4: (Key, DataDefinitionOp[D])
    val op5: (Key, DataDefinitionOp[E])
    val op6: (Key, DataDefinitionOp[F])
    val op7: (Key, DataDefinitionOp[G])
    val op8: (Key, DataDefinitionOp[H])
    val op9: (Key, DataDefinitionOp[I])
    val op10: (Key, DataDefinitionOp[J])
    val op11: (Key, DataDefinitionOp[K])
    val op12: (Key, DataDefinitionOp[L])

    def extractMembers(f: FunctionK[DataDefinitionOp, ValidateFromProducer])
    : ValidateFromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] = {
      val r1 = f(op1._2)
      val r2 = f(op2._2)
      val r3 = f(op3._2)
      val r4 = f(op4._2)
      val r5 = f(op5._2)
      val r6 = f(op6._2)
      val r7 = f(op7._2)
      val r8 = f(op8._2)
      val r9 = f(op9._2)
      val r10 = f(op10._2)
      val r11 = f(op11._2)
      val r12 = f(op12._2)
      (jsonProducer: JsonProducer) =>
      {
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
      val res1 = functionK(op1._2)
      val res2 = functionK(op2._2)
      val res3 = functionK(op3._2)
      val res4 = functionK(op4._2)
      val res5 = functionK(op5._2)
      val res6 = functionK(op6._2)
      val res7 = functionK(op7._2)
      val res8 = functionK(op8._2)
      val res9 = functionK(op9._2)
      val res10 = functionK(op10._2)
      val res11 = functionK(op11._2)
      val res12 = functionK(op12._2)
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
        ).mapN( (j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12) =>
          JObject(List(JField(op1._1.name, j1), JField(op2._1.name, j2), JField(op3._1.name, j3),
            JField(op4._1.name, j4), JField(op5._1.name, j5), JField(op6._1.name, j6), JField(op7._1.name, j7),
            JField(op8._1.name, j8), JField(op9._1.name, j9), JField(op10._1.name, j10), JField(op11._1.name, j11), JField(op12._1.name, j12))
          )
        )
      }
    }

    def members: List[(Key, DataDefinitionOp[_])] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)

  }

  /** Represents a required HList with six properties. */
  final case class HList12[A, B, C, D, E, F, G, H, I, J, K, L](
                                                                op1: (Key, DataDefinitionOp[A]),
                                                                op2: (Key, DataDefinitionOp[B]),
                                                                op3: (Key, DataDefinitionOp[C]),
                                                                op4: (Key, DataDefinitionOp[D]),
                                                                op5: (Key, DataDefinitionOp[E]),
                                                                op6: (Key, DataDefinitionOp[F]),
                                                                op7: (Key, DataDefinitionOp[G]),
                                                                op8: (Key, DataDefinitionOp[H]),
                                                                op9: (Key, DataDefinitionOp[I]),
                                                                op10: (Key, DataDefinitionOp[J]),
                                                                op11: (Key, DataDefinitionOp[K]),
                                                                op12: (Key, DataDefinitionOp[L]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil]
      with ExtractHList12[A, B, C, D, E, F, G, H, I, J, K, L] {

    def optional() = OptionalHList12(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)

  }

  /** Represents an optional HList with six properties. */
  final case class OptionalHList12[A, B, C, D, E, F, G, H, I, J,K, L](
                                                                       op1: (Key, DataDefinitionOp[A]),
                                                                       op2: (Key, DataDefinitionOp[B]),
                                                                       op3: (Key, DataDefinitionOp[C]),
                                                                       op4: (Key, DataDefinitionOp[D]),
                                                                       op5: (Key, DataDefinitionOp[E]),
                                                                       op6: (Key, DataDefinitionOp[F]),
                                                                       op7: (Key, DataDefinitionOp[G]),
                                                                       op8: (Key, DataDefinitionOp[H]),
                                                                       op9: (Key, DataDefinitionOp[I]),
                                                                       op10: (Key, DataDefinitionOp[J]),
                                                                       op11: (Key, DataDefinitionOp[K]),
                                                                       op12: (Key, DataDefinitionOp[L]))
    extends ToHListDataDefinitionOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil]
      with ExtractHList12[A, B, C, D, E, F, G, H, I, J, K, L] {}
}



