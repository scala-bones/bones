package com.gaia.soy

import cats.arrow.FunctionK
import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._
import com.gaia.soy.compiler.JsonCompiler.FromProducer
import shapeless.Generic

/** Aliases for simplifying the creation of ObjectFieldGroup. */


/** Used to define objects as root objects -- using root extraction key */
trait Obj extends ObjAlias {
  override def key: Key = RootKey
}

/** Convenient ways to declare Object specification */
trait ObjAlias {

  import Obj._

  def key: Key

  def obj2[A,B](
    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]]
  ) = Obj2(key, op1, op2)

  def obj3[A,B,C] (
    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]]
  ) = Obj3(key, op1, op2, op3)

//
//  def obj4[A,B,C,D,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]],
//    f: (A,B,C,D) => Z
//  ) = Obj4(key, op1, op2, op3, op4,f)
//
//  def obj5[A,B,C,D,E,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]],
//    f: (A,B,C,D,E) => Z
//  ) = Obj5(key, op1, op2, op3, op4, op5, f)
//
//  def obj6[A,B,C,D,E,F, Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    f: (A,B,C,D,E,F) => Z
//  ) = Obj6(key, op1, op2, op3, op4, op5, op6, f)
//
//  def obj7[A,B,C,D,E,F,G,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    op7: FieldGroupOp[ValidationResultNel[G]],
//    f: (A,B,C,D,E,F,G) => Z
//  ) = Obj7(key, op1, op2, op3, op4, op5, op6, op7, f)
//
//  def obj8[A,B,C,D,E,F,G,H,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    op7: FieldGroupOp[ValidationResultNel[G]], op8: FieldGroupOp[ValidationResultNel[H]],
//    f: (A,B,C,D,E,F,G,H) => Z
//  ) = Obj8(key, op1, op2, op3, op4, op5, op6, op7, op8, f)
//
//  def obj9[A,B,C,D,E,F,G,H,I, Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    op7: FieldGroupOp[ValidationResultNel[G]], op8: FieldGroupOp[ValidationResultNel[H]], op9: FieldGroupOp[ValidationResultNel[I]],
//    f: (A,B,C,D,E,F,G,H,I) => Z
//  ) = Obj9(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, f)
//
//  def obj10[A,B,C,D,E,F,G,H,I,J,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    op7: FieldGroupOp[ValidationResultNel[G]], op8: FieldGroupOp[ValidationResultNel[H]], op9: FieldGroupOp[ValidationResultNel[I]],
//    op10: FieldGroupOp[ValidationResultNel[J]],
//    f: (A,B,C,D,E,F,G,H,I,J) => Z
//  ) = Obj10(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, f)
//
//  def obj11[A,B,C,D,E,F,G,H,I,J,K,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    op7: FieldGroupOp[ValidationResultNel[G]], op8: FieldGroupOp[ValidationResultNel[H]], op9: FieldGroupOp[ValidationResultNel[I]],
//    op10: FieldGroupOp[ValidationResultNel[J]], op11: FieldGroupOp[ValidationResultNel[K]],
//    f: (A,B,C,D,E,F,G,H,I,J,K) => Z
//  ) = Obj11(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, f)
//
//  def obj12[A,B,C,D,E,F,G,H,I,J,K,L,Z](
//    op1: FieldGroupOp[ValidationResultNel[A]], op2: FieldGroupOp[ValidationResultNel[B]], op3: FieldGroupOp[ValidationResultNel[C]],
//    op4: FieldGroupOp[ValidationResultNel[D]], op5: FieldGroupOp[ValidationResultNel[E]], op6: FieldGroupOp[ValidationResultNel[F]],
//    op7: FieldGroupOp[ValidationResultNel[G]], op8: FieldGroupOp[ValidationResultNel[H]], op9: FieldGroupOp[ValidationResultNel[I]],
//    op10: FieldGroupOp[ValidationResultNel[J]],op11: FieldGroupOp[ValidationResultNel[K]], op12: FieldGroupOp[ValidationResultNel[L]],
//    f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z
//  ) = Obj12(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, f)

}

object Obj {

  type FG[A] = FieldGroup[ValidationResultNel[A]]
  type FGO[A] = FieldGroupOp[ValidationResultNel[A]]

  /** Used to create a generic extract method if we can extract values from the products. */
  abstract class ObjectFieldGroup[AA,ZZ] {

    /** The key used to extract the value from the JsonProducer */
    def key: Key
    /** Extract the child or children.  AA should be a Tuple*/
    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[AA]
    /** */
    def tupledF: AA => ZZ

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(producer: JsonProducer): Validated[ExtractionErrors, ZZ] = {
      producer.produceObject(key).leftMap(NonEmptyList.one).toValidated.andThen {
        case Some(producer) => extractChildren(producer).map(tupledF)
        case None => Invalid(NonEmptyList.one(RequiredObjectError(key)))
      }
    }
  }

  /** Used to create a generic extract method if we can extract optional values from the products. */
  abstract class OptionalObjectFieldGroup[AA,ZZ] {
    /** The key used to extract the value from the JsonProducer */
    def key: Key
    /** Extract the child or children.  AA should be a Tuple*/
    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[AA]
    /** */
    def tupledF: AA => ZZ

    /** This will be used to implement the FieldGroupOp in the context of the children. */
    def extract(producer: JsonProducer): Validated[ExtractionErrors, Option[ZZ]] = {
      producer.produceObject(key).leftMap(NonEmptyList.one).toValidated.andThen {
        case Some(producer) => extractChildren(producer).map(t => Some(tupledF(t)))
        case None => Valid(None)
      }
    }
  }

  /** Represents a required object with two properties A and B */
  case class Obj2[A,B](key: Key,
                         op1: FieldGroupOp[ValidationResultNel[A]],
                         op2: FieldGroupOp[ValidationResultNel[B]])
    extends FieldGroupOp[ValidationResultNel[(A,B)]] {

//    def tupledF = f.tupled

//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B)] = {
//      ( op1.extract(jsonProducer), op2.extract(jsonProducer)).mapN( (_,_) )
//    }

    def optional() = OptionalObj2(key, op1, op2)

  }

  /** Represents an optional object with two properties A and B */
  case class OptionalObj2[A,B](key: Key,
                                 op1: FieldGroupOp[ValidationResultNel[A]],
                                 op2: FieldGroupOp[ValidationResultNel[B]])
    extends FieldGroupOp[ValidationResultNel[Option[(A,B)]]] {

//    def tupledF = f.tupled

//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B)] = {
//      ( op1.extract(jsonProducer), op2.extract(jsonProducer)).mapN( (_,_) )
//    }

  }

//  /** Represents a required object with three properties A,B,C */
  case class Obj3[A,B,C](key: Key,
                           op1: FieldGroupOp[ValidationResultNel[A]],
                           op2: FieldGroupOp[ValidationResultNel[B]],
                           op3: FieldGroupOp[ValidationResultNel[C]])
      extends FieldGroupOp[ValidationResultNel[(A,B,C)]] {
//    extends ObjectFieldGroup[(A,B,C),Z] with FieldGroupOp[ValidationResultNel[Z]] {

//    def tupledF = f.tupled

    def extract[T](f: FunctionK[FieldGroupOp, FromProducer]): FromProducer[ValidationResultNel[(A,B,C)]] = {
      val r1 = f.apply(op1)
      val r2 = f.apply(op2)
      val r3 = f.apply(op3)
      (jsonProducer: JsonProducer) => (r1(jsonProducer), r2(jsonProducer), r3(jsonProducer)).mapN( (_,_,_))

    }

//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C)] = {
//      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer)).mapN( (_,_,_) )
//    }

    def optional() = OptionalObj3(key, op1, op2, op3)
    def wrapInClass[Z](implicit gen: Generic.Aux[Z,(A,B,C)]) : WrapInClass[A,B,C,Z] = WrapInClass[A,B,C,Z](this, gen)

  }

  case class WrapInClass[A,B,C,Z](obj3: Obj3[A,B,C], gen: Generic.Aux[Z,(A,B,C)]) {

    import shapeless._
    import syntax.std.tuple._

    def extract[T](f: FunctionK[FieldGroupOp, FromProducer]): FromProducer[ValidationResultNel[Z]] = {
      val tuples = f.apply(obj3)
      (jsonProducer: JsonProducer) => {
        tuples(jsonProducer).map(gen.from)
      }

    }

  }

  /** Represents an optional object with three properties A,B,C */
  case class OptionalObj3[A,B,C](key: Key,
                                   op1: FieldGroupOp[ValidationResultNel[A]],
                                   op2: FieldGroupOp[ValidationResultNel[B]],
                                   op3: FieldGroupOp[ValidationResultNel[C]])
      extends FieldGroupOp[ValidationResultNel[Option[(A,B,C)]]] {
//    extends OptionalObjectFieldGroup[(A,B,C),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {

//    def tupledF = f.tupled

//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C)] = {
//      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer)).mapN( (_,_,_) )
//    }

  }
//
//  /** Represents a required object with four properties A,B,C,D */
//  case class Obj4[A,B,C,D,Z](key: Key,
//                             op1: FieldGroupOp[ValidationResultNel[A]],
//                             op2: FieldGroupOp[ValidationResultNel[B]],
//                             op3: FieldGroupOp[ValidationResultNel[C]],
//                             op4: FieldGroupOp[ValidationResultNel[D]],
//                             f: (A,B,C,D) => Z)
//    extends ObjectFieldGroup[(A,B,C,D),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj4(key, op1, op2, op3, op4, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer)
//      ).mapN( (_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object with three properties A,B,C */
//  case class OptionalObj4[A,B,C,D,Z](key: Key,
//                                     op1: FieldGroupOp[ValidationResultNel[A]],
//                                     op2: FieldGroupOp[ValidationResultNel[B]],
//                                     op3: FieldGroupOp[ValidationResultNel[C]],
//                                     op4: FieldGroupOp[ValidationResultNel[D]],
//                                     f: (A,B,C,D) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer)).mapN( (_,_,_,_)
//      )
//    }
//  }
//
//  /** Represents a required object with five properties A,B,C,D,E */
//  case class Obj5[A,B,C,D,E,Z](key: Key,
//                             op1: FieldGroupOp[ValidationResultNel[A]],
//                             op2: FieldGroupOp[ValidationResultNel[B]],
//                             op3: FieldGroupOp[ValidationResultNel[C]],
//                             op4: FieldGroupOp[ValidationResultNel[D]],
//                             op5: FieldGroupOp[ValidationResultNel[E]],
//                             f: (A,B,C,D,E) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj5(key, op1, op2, op3, op4, op5, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object with five properties A,B,C,D,E */
//  case class OptionalObj5[A,B,C,D,E,Z](key: Key,
//                                     op1: FieldGroupOp[ValidationResultNel[A]],
//                                     op2: FieldGroupOp[ValidationResultNel[B]],
//                                     op3: FieldGroupOp[ValidationResultNel[C]],
//                                     op4: FieldGroupOp[ValidationResultNel[D]],
//                                     op5: FieldGroupOp[ValidationResultNel[E]],
//                                     f: (A,B,C,D,E) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with six properties A,B,C,D,E,F */
//  case class Obj6[A,B,C,D,E,F,Z](key: Key,
//                               op1: FieldGroupOp[ValidationResultNel[A]],
//                               op2: FieldGroupOp[ValidationResultNel[B]],
//                               op3: FieldGroupOp[ValidationResultNel[C]],
//                               op4: FieldGroupOp[ValidationResultNel[D]],
//                               op5: FieldGroupOp[ValidationResultNel[E]],
//                               op6: FieldGroupOp[ValidationResultNel[F]],
//                               f: (A,B,C,D,E,F) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj6(key, op1, op2, op3, op4, op5, op6, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with six properties A,B,C,D,E,F */
//  case class OptionalObj6[A,B,C,D,E,F,Z](key: Key,
//                                       op1: FieldGroupOp[ValidationResultNel[A]],
//                                       op2: FieldGroupOp[ValidationResultNel[B]],
//                                       op3: FieldGroupOp[ValidationResultNel[C]],
//                                       op4: FieldGroupOp[ValidationResultNel[D]],
//                                       op5: FieldGroupOp[ValidationResultNel[E]],
//                                       op6: FieldGroupOp[ValidationResultNel[F]],
//                                       f: (A,B,C,D,E,F) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with seven properties A,B,C,D,E,F,G */
//  case class Obj7[A,B,C,D,E,F,G,Z](key: Key,
//                                 op1: FieldGroupOp[ValidationResultNel[A]],
//                                 op2: FieldGroupOp[ValidationResultNel[B]],
//                                 op3: FieldGroupOp[ValidationResultNel[C]],
//                                 op4: FieldGroupOp[ValidationResultNel[D]],
//                                 op5: FieldGroupOp[ValidationResultNel[E]],
//                                 op6: FieldGroupOp[ValidationResultNel[F]],
//                                 op7: FieldGroupOp[ValidationResultNel[G]],
//                                 f: (A,B,C,D,E,F,G) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F,G),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj7(key, op1, op2, op3, op4, op5, op6, op7, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with six properties A,B,C,D,E,F */
//  case class OptionalObj7[A,B,C,D,E,F,G,Z](key: Key,
//                                         op1: FieldGroupOp[ValidationResultNel[A]],
//                                         op2: FieldGroupOp[ValidationResultNel[B]],
//                                         op3: FieldGroupOp[ValidationResultNel[C]],
//                                         op4: FieldGroupOp[ValidationResultNel[D]],
//                                         op5: FieldGroupOp[ValidationResultNel[E]],
//                                         op6: FieldGroupOp[ValidationResultNel[F]],
//                                         op7: FieldGroupOp[ValidationResultNel[G]],
//                                         f: (A,B,C,D,E,F,G) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F,G),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with eight properties A,B,C,D,E,F,G,H */
//  case class Obj8[A,B,C,D,E,F,G,H,Z](key: Key,
//                                   op1: FieldGroupOp[ValidationResultNel[A]],
//                                   op2: FieldGroupOp[ValidationResultNel[B]],
//                                   op3: FieldGroupOp[ValidationResultNel[C]],
//                                   op4: FieldGroupOp[ValidationResultNel[D]],
//                                   op5: FieldGroupOp[ValidationResultNel[E]],
//                                   op6: FieldGroupOp[ValidationResultNel[F]],
//                                   op7: FieldGroupOp[ValidationResultNel[G]],
//                                   op8: FieldGroupOp[ValidationResultNel[H]],
//                                   f: (A,B,C,D,E,F,G,H) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F,G,H),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj8(key, op1, op2, op3, op4, op5, op6, op7, op8, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with six properties A,B,C,D,E,F */
//  case class OptionalObj8[A,B,C,D,E,F,G,H,Z](key: Key,
//                                           op1: FieldGroupOp[ValidationResultNel[A]],
//                                           op2: FieldGroupOp[ValidationResultNel[B]],
//                                           op3: FieldGroupOp[ValidationResultNel[C]],
//                                           op4: FieldGroupOp[ValidationResultNel[D]],
//                                           op5: FieldGroupOp[ValidationResultNel[E]],
//                                           op6: FieldGroupOp[ValidationResultNel[F]],
//                                           op7: FieldGroupOp[ValidationResultNel[G]],
//                                           op8: FieldGroupOp[ValidationResultNel[H]],
//                                           f: (A,B,C,D,E,F,G,H) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F,G,H),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with eight properties A,B,C,D,E,F,G,H */
//  case class Obj9[A,B,C,D,E,F,G,H,I,Z](key: Key,
//                                     op1: FieldGroupOp[ValidationResultNel[A]],
//                                     op2: FieldGroupOp[ValidationResultNel[B]],
//                                     op3: FieldGroupOp[ValidationResultNel[C]],
//                                     op4: FieldGroupOp[ValidationResultNel[D]],
//                                     op5: FieldGroupOp[ValidationResultNel[E]],
//                                     op6: FieldGroupOp[ValidationResultNel[F]],
//                                     op7: FieldGroupOp[ValidationResultNel[G]],
//                                     op8: FieldGroupOp[ValidationResultNel[H]],
//                                     op9: FieldGroupOp[ValidationResultNel[I]],
//                                     f: (A,B,C,D,E,F,G,H,I) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F,G,H,I),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj9(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with six properties A,B,C,D,E,F */
//  case class OptionalObj9[A,B,C,D,E,F,G,H,I,Z](key: Key,
//                                             op1: FieldGroupOp[ValidationResultNel[A]],
//                                             op2: FieldGroupOp[ValidationResultNel[B]],
//                                             op3: FieldGroupOp[ValidationResultNel[C]],
//                                             op4: FieldGroupOp[ValidationResultNel[D]],
//                                             op5: FieldGroupOp[ValidationResultNel[E]],
//                                             op6: FieldGroupOp[ValidationResultNel[F]],
//                                             op7: FieldGroupOp[ValidationResultNel[G]],
//                                             op8: FieldGroupOp[ValidationResultNel[H]],
//                                             op9: FieldGroupOp[ValidationResultNel[I]],
//                                             f: (A,B,C,D,E,F,G,H,I) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F,G,H,I),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with ten properties A,B,C,D,E,F,G,H,I,J */
//  case class Obj10[A,B,C,D,E,F,G,H,I,J,Z](key: Key,
//                                       op1: FieldGroupOp[ValidationResultNel[A]],
//                                       op2: FieldGroupOp[ValidationResultNel[B]],
//                                       op3: FieldGroupOp[ValidationResultNel[C]],
//                                       op4: FieldGroupOp[ValidationResultNel[D]],
//                                       op5: FieldGroupOp[ValidationResultNel[E]],
//                                       op6: FieldGroupOp[ValidationResultNel[F]],
//                                       op7: FieldGroupOp[ValidationResultNel[G]],
//                                       op8: FieldGroupOp[ValidationResultNel[H]],
//                                       op9: FieldGroupOp[ValidationResultNel[I]],
//                                       op10: FieldGroupOp[ValidationResultNel[J]],
//                                       f: (A,B,C,D,E,F,G,H,I,J) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F,G,H,I,J),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj10(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I,J)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer),
//        op10.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with 10 properties A,B,C,D,E,F */
//  case class OptionalObj10[A,B,C,D,E,F,G,H,I,J,Z](key: Key,
//                                               op1: FieldGroupOp[ValidationResultNel[A]],
//                                               op2: FieldGroupOp[ValidationResultNel[B]],
//                                               op3: FieldGroupOp[ValidationResultNel[C]],
//                                               op4: FieldGroupOp[ValidationResultNel[D]],
//                                               op5: FieldGroupOp[ValidationResultNel[E]],
//                                               op6: FieldGroupOp[ValidationResultNel[F]],
//                                               op7: FieldGroupOp[ValidationResultNel[G]],
//                                               op8: FieldGroupOp[ValidationResultNel[H]],
//                                               op9: FieldGroupOp[ValidationResultNel[I]],
//                                               op10: FieldGroupOp[ValidationResultNel[J]],
//                                               f: (A,B,C,D,E,F,G,H,I,J) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F,G,H,I,J),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I,J)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer),
//        op10.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with ten properties A,B,C,D,E,F,G,H,I,J */
//  case class Obj11[A,B,C,D,E,F,G,H,I,J,K,Z](key: Key,
//                                          op1: FieldGroupOp[ValidationResultNel[A]],
//                                          op2: FieldGroupOp[ValidationResultNel[B]],
//                                          op3: FieldGroupOp[ValidationResultNel[C]],
//                                          op4: FieldGroupOp[ValidationResultNel[D]],
//                                          op5: FieldGroupOp[ValidationResultNel[E]],
//                                          op6: FieldGroupOp[ValidationResultNel[F]],
//                                          op7: FieldGroupOp[ValidationResultNel[G]],
//                                          op8: FieldGroupOp[ValidationResultNel[H]],
//                                          op9: FieldGroupOp[ValidationResultNel[I]],
//                                          op10: FieldGroupOp[ValidationResultNel[J]],
//                                          op11: FieldGroupOp[ValidationResultNel[K]],
//                                          f: (A,B,C,D,E,F,G,H,I,J,K) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F,G,H,I,J,K),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj11(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I,J,K)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer),
//        op10.extract(jsonProducer), op11.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with six properties A,B,C,D,E,F */
//  case class OptionalObj11[A,B,C,D,E,F,G,H,I,J,K,Z](key: Key,
//                                                  op1: FieldGroupOp[ValidationResultNel[A]],
//                                                  op2: FieldGroupOp[ValidationResultNel[B]],
//                                                  op3: FieldGroupOp[ValidationResultNel[C]],
//                                                  op4: FieldGroupOp[ValidationResultNel[D]],
//                                                  op5: FieldGroupOp[ValidationResultNel[E]],
//                                                  op6: FieldGroupOp[ValidationResultNel[F]],
//                                                  op7: FieldGroupOp[ValidationResultNel[G]],
//                                                  op8: FieldGroupOp[ValidationResultNel[H]],
//                                                  op9: FieldGroupOp[ValidationResultNel[I]],
//                                                  op10: FieldGroupOp[ValidationResultNel[J]],
//                                                  op11: FieldGroupOp[ValidationResultNel[K]],
//                                                  f: (A,B,C,D,E,F,G,H,I,J,K) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F,G,H,I,J,K),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I,J,K)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer),
//        op10.extract(jsonProducer), op11.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_,_,_) )
//    }
//  }
//
//  /** Represents a required object Z with ten properties A,B,C,D,E,F,G,H,I,J */
//  case class Obj12[A,B,C,D,E,F,G,H,I,J,K,L,Z](key: Key,
//                                            op1: FieldGroupOp[ValidationResultNel[A]],
//                                            op2: FieldGroupOp[ValidationResultNel[B]],
//                                            op3: FieldGroupOp[ValidationResultNel[C]],
//                                            op4: FieldGroupOp[ValidationResultNel[D]],
//                                            op5: FieldGroupOp[ValidationResultNel[E]],
//                                            op6: FieldGroupOp[ValidationResultNel[F]],
//                                            op7: FieldGroupOp[ValidationResultNel[G]],
//                                            op8: FieldGroupOp[ValidationResultNel[H]],
//                                            op9: FieldGroupOp[ValidationResultNel[I]],
//                                            op10: FieldGroupOp[ValidationResultNel[J]],
//                                            op11: FieldGroupOp[ValidationResultNel[K]],
//                                            op12: FieldGroupOp[ValidationResultNel[L]],
//                                            f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z)
//    extends ObjectFieldGroup[(A,B,C,D,E,F,G,H,I,J,K,L),Z] with FieldGroupOp[ValidationResultNel[Z]] {
//
//    def tupledF = f.tupled
//
//    def optional() = OptionalObj12(key, op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12, f)
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I,J,K,L)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer),
//        op10.extract(jsonProducer), op11.extract(jsonProducer), op12.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_,_,_,_) )
//    }
//
//  }
//
//  /** Represents an optional object Z with six properties A,B,C,D,E,F */
//  case class OptionalObj12[A,B,C,D,E,F,G,H,I,J,K,L,Z](key: Key,
//                                                    op1: FieldGroupOp[ValidationResultNel[A]],
//                                                    op2: FieldGroupOp[ValidationResultNel[B]],
//                                                    op3: FieldGroupOp[ValidationResultNel[C]],
//                                                    op4: FieldGroupOp[ValidationResultNel[D]],
//                                                    op5: FieldGroupOp[ValidationResultNel[E]],
//                                                    op6: FieldGroupOp[ValidationResultNel[F]],
//                                                    op7: FieldGroupOp[ValidationResultNel[G]],
//                                                    op8: FieldGroupOp[ValidationResultNel[H]],
//                                                    op9: FieldGroupOp[ValidationResultNel[I]],
//                                                    op10: FieldGroupOp[ValidationResultNel[J]],
//                                                    op11: FieldGroupOp[ValidationResultNel[K]],
//                                                    op12: FieldGroupOp[ValidationResultNel[L]],
//                                                    f: (A,B,C,D,E,F,G,H,I,J,K,L) => Z)
//    extends OptionalObjectFieldGroup[(A,B,C,D,E,F,G,H,I,J,K,L),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {
//
//    def tupledF = f.tupled
//
//    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D,E,F,G,H,I,J,K,L)] = {
//      (
//        op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer),
//        op4.extract(jsonProducer), op5.extract(jsonProducer), op6.extract(jsonProducer),
//        op7.extract(jsonProducer), op8.extract(jsonProducer), op9.extract(jsonProducer),
//        op10.extract(jsonProducer), op11.extract(jsonProducer), op12.extract(jsonProducer)
//      ).mapN( (_,_,_,_,_,_,_,_,_,_,_,_) )
//    }
//  }

}

