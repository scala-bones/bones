package com.ot.bones.validation

import cats.arrow.FunctionK
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._
import com.ot.bones.compiler.ExtractionCompiler.{
  FromProducer,
  JsonProducer,
  RequiredObjectError
}
import com.ot.bones.{BonesOp, Key, RootKey}
import shapeless._

/** Aliases for simplifying the creation of ObjectFieldGroup. */
/** Convenient ways to declare Object specification */
trait ObjAlias {

  import ToHList._

  def key: Key

  def obj2[A, B](op1: BonesOp[A], op2: BonesOp[B]) = HList2(key, op1, op2)

  def obj3[A, B, C](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C]) =
    HList3(key, op1, op2, op3)

  def obj4[A, B, C, D](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D]) =
    HList4(key, op1, op2, op3, op4)

  def obj5[A, B, C, D, E](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E]) =
    HList5(key, op1, op2, op3, op4, op5)

  def obj6[A, B, C, D, E, F](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                             op6: BonesOp[F]) =
    HList6(key, op1, op2, op3, op4, op5, op6)

  def obj7[A, B, C, D, E, F, G](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                             op6: BonesOp[F], op7: BonesOp[G]) =
    HList7(key, op1, op2, op3, op4, op5, op6, op7)

  def obj8[A, B, C, D, E, F, G, H](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                                op6: BonesOp[F], op7: BonesOp[G], op8: BonesOp[H]) =
    HList8(key, op1, op2, op3, op4, op5, op6, op7, op8)

  def obj9[A, B, C, D, E, F, G, H, I](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                                   op6: BonesOp[F], op7: BonesOp[G], op8: BonesOp[H], op9: BonesOp[I]) =
    HList9(key, op1, op2, op3, op4, op5, op6, op7, op8, op9)

  def obj10[A, B, C, D, E, F, G, H, I, J](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                                      op6: BonesOp[F], op7: BonesOp[G], op8: BonesOp[H], op9: BonesOp[I], op10: BonesOp[J]) =
    HList10(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  def obj11[A, B, C, D, E, F, G, H, I, J, K](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                                          op6: BonesOp[F], op7: BonesOp[G], op8: BonesOp[H], op9: BonesOp[I], op10: BonesOp[J],
                                             op11: BonesOp[K]) =
    HList11(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  def obj12[A, B, C, D, E, F, G, H, I, J, K, L](op1: BonesOp[A], op2: BonesOp[B], op3: BonesOp[C], op4: BonesOp[D], op5: BonesOp[E],
                                             op6: BonesOp[F], op7: BonesOp[G], op8: BonesOp[H], op9: BonesOp[I], op10: BonesOp[J],
                                             op11: BonesOp[K], op12: BonesOp[L]) =
    HList12(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)



}

/** Used to define objects as root objects -- using root extraction key */
trait ToHList extends ObjAlias {
  override def key: Key = RootKey
}

object ToHList {

  /** Used to create a generic extract method if we can extract values from the products. */
  abstract class ToHListBonesOp[L <: HList] extends BonesOp[L] {

    /** The key used to extract the value from the JsonProducer */
    def key: Key

    /** Get a list of untyped members */
    def members: List[BonesOp[_]]

    /** Extract the child or children.  AA should be a Tuple */
    def extractMembers(
        functionK: FunctionK[BonesOp, FromProducer]): FromProducer[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(
        functionK: FunctionK[BonesOp, FromProducer]): FromProducer[L] = {
      val res = functionK(key)
      (json: JsonProducer) =>
        {
          res(json).andThen {
            case Some(producer) => extractMembers(functionK)(producer)
            case None           => Invalid(NonEmptyList.one(RequiredObjectError(key)))
          }
        }
    }
  }

  /** Used to create a generic extract method if we can extract optional values from the products. */
  abstract class ToOptionalHListBonesOp[L <: HList] extends BonesOp[Option[L]] {

    /** The key used to extract the value from the JsonProducer */
    def key: Key

    def members: List[BonesOp[_]]

    /** Extract the child or children.  AA should be a Tuple */
    def extractMembers(
        functionK: FunctionK[BonesOp, FromProducer]): FromProducer[L]

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(functionK: FunctionK[BonesOp, FromProducer])
      : FromProducer[Option[L]] = {
      val res = functionK(key)
      (json: JsonProducer) =>
        {
          res(json).andThen {
            case Some(producer) =>
              extractMembers(functionK)(producer).map(Some(_))
            case None => Valid(None)
          }
        }
    }
  }

  /** Base trait for HList2 */
  trait ExtractHList2[A, B] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]

    def extractMembers(functionK: FunctionK[BonesOp, FromProducer])
      : FromProducer[A :: B :: HNil] = {
      val res1 = functionK(op1)
      val res2 = functionK(op2)
      (jsonProducer: JsonProducer) =>
        {
          (res1(jsonProducer), res2(jsonProducer))
            .mapN(_ :: _ :: HNil)
        }
    }

    def members: List[BonesOp[_]] = List(op1, op2)

  }

  /** Represents a required HList with two properties A and B */
  case class HList2[A, B](key: Key, op1: BonesOp[A], op2: BonesOp[B])
      extends ToHListBonesOp[A :: B :: HNil]
      with ExtractHList2[A, B] {

    def optional() = ToOptionalHList2(key, op1, op2)

  }

  /** Represents an optional object with two properties A and B */
  case class ToOptionalHList2[A, B](key: Key, op1: BonesOp[A], op2: BonesOp[B])
      extends ToOptionalHListBonesOp[A :: B :: HNil]
      with ExtractHList2[A, B] {}

  /** Base trait contains common functionality for extracting an HList with 3 elements */
  trait ExtractHList3[A, B, C] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
      : FromProducer[A :: B :: C :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      (jsonProducer: JsonProducer) =>
        {
          (r1(jsonProducer), r2(jsonProducer), r3(jsonProducer))
            .mapN(_ :: _ :: _ :: HNil)
        }

    }

    def members: List[BonesOp[_]] = List(op1, op2, op3)

  }

  /** Represents a required HList with three properties A,B,C */
  case class HList3[A, B, C](key: Key,
                             op1: BonesOp[A],
                             op2: BonesOp[B],
                             op3: BonesOp[C])
      extends ToHListBonesOp[A :: B :: C :: HNil]
      with ExtractHList3[A, B, C] {

    def optional() = OptionalHList3(key, op1, op2, op3)

  }

  /** Represents an optional HList with three properties A,B,C */
  case class OptionalHList3[A, B, C](key: Key,
                                     op1: BonesOp[A],
                                     op2: BonesOp[B],
                                     op3: BonesOp[C])
      extends ToHListBonesOp[A :: B :: C :: HNil]
      with ExtractHList3[A, B, C] {}

  /** Base trait contains common functionality for extracting an HList with 4 elements */
  trait ExtractHList4[A, B, C, D] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
      : FromProducer[A :: B :: C :: D :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      (jsonProducer: JsonProducer) =>
        {
          (r1(jsonProducer),
           r2(jsonProducer),
           r3(jsonProducer),
           r4(jsonProducer)).mapN(_ :: _ :: _ :: _ :: HNil)
        }
    }

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4)

  }

  /** Represents a required HList with four properties A,B,C,D */
  case class HList4[A, B, C, D](key: Key,
                                op1: BonesOp[A],
                                op2: BonesOp[B],
                                op3: BonesOp[C],
                                op4: BonesOp[D])
      extends ToHListBonesOp[A :: B :: C :: D :: HNil]
      with ExtractHList4[A, B, C, D] {

    def optional() = OptionalHList4(key, op1, op2, op3, op4)

  }

  /** Represents an optional HList with four properties A,B,C,D */
  case class OptionalHList4[A, B, C, D](key: Key,
                                        op1: BonesOp[A],
                                        op2: BonesOp[B],
                                        op3: BonesOp[C],
                                        op4: BonesOp[D])
      extends ToHListBonesOp[A :: B :: C :: D :: HNil]
      with ExtractHList4[A, B, C, D] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList5[A, B, C, D, E] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
      : FromProducer[A :: B :: C :: D :: E :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      (jsonProducer: JsonProducer) =>
        {
          (r1(jsonProducer),
           r2(jsonProducer),
           r3(jsonProducer),
           r4(jsonProducer),
           r5(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: HNil)
        }
    }

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op4)

  }

  /** Represents a required HList with four properties. */
  case class HList5[A, B, C, D, E](key: Key,
                                   op1: BonesOp[A],
                                   op2: BonesOp[B],
                                   op3: BonesOp[C],
                                   op4: BonesOp[D],
                                   op5: BonesOp[E])
      extends ToHListBonesOp[A :: B :: C :: D :: E :: HNil]
      with ExtractHList5[A, B, C, D, E] {

    def optional() = OptionalHList5(key, op1, op2, op3, op4, op5)

  }

  /** Represents an optional HList with five properties. */
  case class OptionalHList5[A, B, C, D, E](key: Key,
                                           op1: BonesOp[A],
                                           op2: BonesOp[B],
                                           op3: BonesOp[C],
                                           op4: BonesOp[D],
                                           op5: BonesOp[E])
      extends ToHListBonesOp[A :: B :: C :: D :: E :: HNil]
      with ExtractHList5[A, B, C, D, E] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList6[A, B, C, D, E, F] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
      : FromProducer[A :: B :: C :: D :: E :: F :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
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

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6)

  }

  /** Represents a required HList with six properties. */
  case class HList6[A, B, C, D, E, F](key: Key,
                                      op1: BonesOp[A],
                                      op2: BonesOp[B],
                                      op3: BonesOp[C],
                                      op4: BonesOp[D],
                                      op5: BonesOp[E],
                                      op6: BonesOp[F])
      extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: HNil]
      with ExtractHList6[A, B, C, D, E, F] {

    def optional() = OptionalHList6(key, op1, op2, op3, op4, op5, op6)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList6[A, B, C, D, E, F](key: Key,
                                              op1: BonesOp[A],
                                              op2: BonesOp[B],
                                              op3: BonesOp[C],
                                              op4: BonesOp[D],
                                              op5: BonesOp[E],
                                              op6: BonesOp[F])
      extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: HNil]
      with ExtractHList6[A, B, C, D, E, F] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList7[A, B, C, D, E, F, G] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]
    val op7: BonesOp[G]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
    : FromProducer[A :: B :: C :: D :: E :: F :: G :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
      val r7 = f(op7)
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

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6, op7)

  }

  /** Represents a required HList with six properties. */
  case class HList7[A, B, C, D, E, F, G](key: Key,
                                      op1: BonesOp[A],
                                      op2: BonesOp[B],
                                      op3: BonesOp[C],
                                      op4: BonesOp[D],
                                      op5: BonesOp[E],
                                      op6: BonesOp[F],
                                     op7: BonesOp[G]  )
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: HNil]
      with ExtractHList7[A, B, C, D, E, F, G] {

    def optional() = OptionalHList7(key, op1, op2, op3, op4, op5, op6, op7)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList7[A, B, C, D, E, F, G](key: Key,
                                              op1: BonesOp[A],
                                              op2: BonesOp[B],
                                              op3: BonesOp[C],
                                              op4: BonesOp[D],
                                              op5: BonesOp[E],
                                              op6: BonesOp[F],
                                               op7: BonesOp[G]  )
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: HNil]
      with ExtractHList7[A, B, C, D, E, F, G] {}


  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList8[A, B, C, D, E, F, G, H] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]
    val op7: BonesOp[G]
    val op8: BonesOp[H]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
    : FromProducer[A :: B :: C :: D :: E :: F :: G :: H :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
      val r7 = f(op7)
      val r8 = f(op8)
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

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8)

  }

  /** Represents a required HList with six properties. */
  case class HList8[A, B, C, D, E, F, G, H](key: Key,
                                         op1: BonesOp[A],
                                         op2: BonesOp[B],
                                         op3: BonesOp[C],
                                         op4: BonesOp[D],
                                         op5: BonesOp[E],
                                         op6: BonesOp[F],
                                         op7: BonesOp[G],
                                            op8: BonesOp[H])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: HNil]
      with ExtractHList8[A, B, C, D, E, F, G, H] {

    def optional() = OptionalHList8(key, op1, op2, op3, op4, op5, op6, op7, op8)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList8[A, B, C, D, E, F, G, H](key: Key,
                                                 op1: BonesOp[A],
                                                 op2: BonesOp[B],
                                                 op3: BonesOp[C],
                                                 op4: BonesOp[D],
                                                 op5: BonesOp[E],
                                                 op6: BonesOp[F],
                                                 op7: BonesOp[G],
                                                  op8: BonesOp[H])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: HNil]
      with ExtractHList8[A, B, C, D, E, F, G, H] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList9[A, B, C, D, E, F, G, H, I] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]
    val op7: BonesOp[G]
    val op8: BonesOp[H]
    val op9: BonesOp[I]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
    : FromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
      val r7 = f(op7)
      val r8 = f(op8)
      val r9 = f(op9)
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
          r9(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9)

  }

  /** Represents a required HList with six properties. */
  case class HList9[A, B, C, D, E, F, G, H, I](key: Key,
                                            op1: BonesOp[A],
                                            op2: BonesOp[B],
                                            op3: BonesOp[C],
                                            op4: BonesOp[D],
                                            op5: BonesOp[E],
                                            op6: BonesOp[F],
                                            op7: BonesOp[G],
                                            op8: BonesOp[H],
                                            op9: BonesOp[I])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil]
      with ExtractHList9[A, B, C, D, E, F, G, H, I] {

    def optional() = OptionalHList9(key, op1, op2, op3, op4, op5, op6, op7, op8, op9)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList9[A, B, C, D, E, F, G, H, I](key: Key,
                                                    op1: BonesOp[A],
                                                    op2: BonesOp[B],
                                                    op3: BonesOp[C],
                                                    op4: BonesOp[D],
                                                    op5: BonesOp[E],
                                                    op6: BonesOp[F],
                                                    op7: BonesOp[G],
                                                    op8: BonesOp[H],
                                                     op9: BonesOp[I])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: HNil]
      with ExtractHList9[A, B, C, D, E, F, G, H, I] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList10[A, B, C, D, E, F, G, H, I, J] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]
    val op7: BonesOp[G]
    val op8: BonesOp[H]
    val op9: BonesOp[I]
    val op10: BonesOp[J]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
    : FromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
      val r7 = f(op7)
      val r8 = f(op8)
      val r9 = f(op9)
      val r10 = f(op10)
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
          r10(jsonProducer)).mapN(_ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: _ :: HNil)
      }
    }

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  }

  /** Represents a required HList with six properties. */
  case class HList10[A, B, C, D, E, F, G, H, I, J](key: Key,
                                               op1: BonesOp[A],
                                               op2: BonesOp[B],
                                               op3: BonesOp[C],
                                               op4: BonesOp[D],
                                               op5: BonesOp[E],
                                               op6: BonesOp[F],
                                               op7: BonesOp[G],
                                               op8: BonesOp[H],
                                               op9: BonesOp[I],
                                               op10: BonesOp[J])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil]
      with ExtractHList10[A, B, C, D, E, F, G, H, I, J] {

    def optional() = OptionalHList10(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList10[A, B, C, D, E, F, G, H, I, J](key: Key,
                                                       op1: BonesOp[A],
                                                       op2: BonesOp[B],
                                                       op3: BonesOp[C],
                                                       op4: BonesOp[D],
                                                       op5: BonesOp[E],
                                                       op6: BonesOp[F],
                                                       op7: BonesOp[G],
                                                       op8: BonesOp[H],
                                                       op9: BonesOp[I],
                                                       op10: BonesOp[J]   )
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: HNil]
      with ExtractHList10[A, B, C, D, E, F, G, H, I, J] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList11[A, B, C, D, E, F, G, H, I, J, K] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]
    val op7: BonesOp[G]
    val op8: BonesOp[H]
    val op9: BonesOp[I]
    val op10: BonesOp[J]
    val op11: BonesOp[K]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
    : FromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
      val r7 = f(op7)
      val r8 = f(op8)
      val r9 = f(op9)
      val r10 = f(op10)
      val r11 = f(op11)
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

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  }

  /** Represents a required HList with six properties. */
  case class HList11[A, B, C, D, E, F, G, H, I, J, K](key: Key,
                                                   op1: BonesOp[A],
                                                   op2: BonesOp[B],
                                                   op3: BonesOp[C],
                                                   op4: BonesOp[D],
                                                   op5: BonesOp[E],
                                                   op6: BonesOp[F],
                                                   op7: BonesOp[G],
                                                   op8: BonesOp[H],
                                                   op9: BonesOp[I],
                                                   op10: BonesOp[J],
                                                   op11: BonesOp[K])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil]
      with ExtractHList11[A, B, C, D, E, F, G, H, I, J, K] {

    def optional() = OptionalHList11(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList11[A, B, C, D, E, F, G, H, I, J,K](key: Key,
                                                           op1: BonesOp[A],
                                                           op2: BonesOp[B],
                                                           op3: BonesOp[C],
                                                           op4: BonesOp[D],
                                                           op5: BonesOp[E],
                                                           op6: BonesOp[F],
                                                           op7: BonesOp[G],
                                                           op8: BonesOp[H],
                                                           op9: BonesOp[I],
                                                           op10: BonesOp[J],
                                                           op11: BonesOp[K] )
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: HNil]
      with ExtractHList11[A, B, C, D, E, F, G, H, I, J, K] {}

  /** Base trait contains common functionality for extracting an HList with 5 elements */
  trait ExtractHList12[A, B, C, D, E, F, G, H, I, J, K, L] {
    val op1: BonesOp[A]
    val op2: BonesOp[B]
    val op3: BonesOp[C]
    val op4: BonesOp[D]
    val op5: BonesOp[E]
    val op6: BonesOp[F]
    val op7: BonesOp[G]
    val op8: BonesOp[H]
    val op9: BonesOp[I]
    val op10: BonesOp[J]
    val op11: BonesOp[K]
    val op12: BonesOp[L]

    def extractMembers(f: FunctionK[BonesOp, FromProducer])
    : FromProducer[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil] = {
      val r1 = f(op1)
      val r2 = f(op2)
      val r3 = f(op3)
      val r4 = f(op4)
      val r5 = f(op5)
      val r6 = f(op6)
      val r7 = f(op7)
      val r8 = f(op8)
      val r9 = f(op9)
      val r10 = f(op10)
      val r11 = f(op11)
      val r12 = f(op12)
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

    def members: List[BonesOp[_]] = List(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)

  }

  /** Represents a required HList with six properties. */
  case class HList12[A, B, C, D, E, F, G, H, I, J, K, L](key: Key,
                                                      op1: BonesOp[A],
                                                      op2: BonesOp[B],
                                                      op3: BonesOp[C],
                                                      op4: BonesOp[D],
                                                      op5: BonesOp[E],
                                                      op6: BonesOp[F],
                                                      op7: BonesOp[G],
                                                      op8: BonesOp[H],
                                                      op9: BonesOp[I],
                                                      op10: BonesOp[J],
                                                      op11: BonesOp[K],
                                                      op12: BonesOp[L])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil]
      with ExtractHList12[A, B, C, D, E, F, G, H, I, J, K, L] {

    def optional() = OptionalHList12(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)

  }

  /** Represents an optional HList with six properties. */
  case class OptionalHList12[A, B, C, D, E, F, G, H, I, J,K, L](key: Key,
                                                             op1: BonesOp[A],
                                                             op2: BonesOp[B],
                                                             op3: BonesOp[C],
                                                             op4: BonesOp[D],
                                                             op5: BonesOp[E],
                                                             op6: BonesOp[F],
                                                             op7: BonesOp[G],
                                                             op8: BonesOp[H],
                                                             op9: BonesOp[I],
                                                             op10: BonesOp[J],
                                                             op11: BonesOp[K],
                                                             op12: BonesOp[L])
    extends ToHListBonesOp[A :: B :: C :: D :: E :: F :: G :: H :: I :: J :: K :: L :: HNil]
      with ExtractHList12[A, B, C, D, E, F, G, H, I, J, K, L] {}
}
