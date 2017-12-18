package com.gaia.soy

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._

/** Aliases for simplifying the creation of ObjectFieldGroup. */
trait ObjAlias {
  import Obj._

  def key: Key

  def obj2[A,B,Z](
                   op1: FieldGroupOp[ValidationResultNel[A]],
                   op2: FieldGroupOp[ValidationResultNel[B]],
                   f: (A,B) => Z
                 ) = Obj2(key, op1, op2, f)


  def obj4[A,B,C,D,Z](
                       op1: FieldGroupOp[ValidationResultNel[A]],
                       op2: FieldGroupOp[ValidationResultNel[B]],
                       op3: FieldGroupOp[ValidationResultNel[C]],
                       op4: FieldGroupOp[ValidationResultNel[D]],
                       f: (A,B,C,D) => Z
                     ) = Obj4(key, op1, op2, op3, op4,f)
}

object Obj extends ObjAlias{

  //Used to
  val key = RootKey

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
  case class Obj2[A,B,Z](key: Key,
                         op1: FieldGroupOp[ValidationResultNel[A]],
                         op2: FieldGroupOp[ValidationResultNel[B]],
                         f: (A,B) => Z)
    extends ObjectFieldGroup[(A,B),Z] with FieldGroupOp[ValidationResultNel[Z]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer)).mapN( (_,_) )
    }

    def optional() = OptionalObj2(key, op1, op2, f)

  }

  /** Represents an optional object with two properties A and B */
  case class OptionalObj2[A,B,Z](key: Key,
                         op1: FieldGroupOp[ValidationResultNel[A]],
                         op2: FieldGroupOp[ValidationResultNel[B]],
                         f: (A,B) => Z)
    extends OptionalObjectFieldGroup[(A,B),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer)).mapN( (_,_) )
    }

  }

  /** Represents a required object with three properties A,B,C */
  case class Obj3[A,B,C,Z](key: Key,
                         op1: FieldGroupOp[ValidationResultNel[A]],
                         op2: FieldGroupOp[ValidationResultNel[B]],
                         op3: FieldGroupOp[ValidationResultNel[C]],
                         f: (A,B,C) => Z)
    extends ObjectFieldGroup[(A,B,C),Z] with FieldGroupOp[ValidationResultNel[Z]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer)).mapN( (_,_,_) )
    }

    def optional() = OptionalObj3(key, op1, op2, op3, f)

  }

  /** Represents an optional object with three properties A,B,C */
  case class OptionalObj3[A,B,C,Z](key: Key,
                                 op1: FieldGroupOp[ValidationResultNel[A]],
                                 op2: FieldGroupOp[ValidationResultNel[B]],
                                 op3: FieldGroupOp[ValidationResultNel[C]],
                                 f: (A,B,C) => Z)
    extends OptionalObjectFieldGroup[(A,B,C),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer)).mapN( (_,_,_) )
    }

  }

  /** Represents a required object with four properties A,B,C,D */
  case class Obj4[A,B,C,D,Z](key: Key,
                             op1: FieldGroupOp[ValidationResultNel[A]],
                             op2: FieldGroupOp[ValidationResultNel[B]],
                             op3: FieldGroupOp[ValidationResultNel[C]],
                             op4: FieldGroupOp[ValidationResultNel[D]],
                             f: (A,B,C,D) => Z)
    extends ObjectFieldGroup[(A,B,C,D),Z] with FieldGroupOp[ValidationResultNel[Z]] {

    def tupledF = f.tupled

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer), op4.extract(jsonProducer))
          .mapN( (_,_,_,_) )
    }

  }

  /** Represents an optional object with three properties A,B,C */
  case class OptionalObj4[A,B,C,D,Z](key: Key,
                                   op1: FieldGroupOp[ValidationResultNel[A]],
                                   op2: FieldGroupOp[ValidationResultNel[B]],
                                   op3: FieldGroupOp[ValidationResultNel[C]],
                                   op4: FieldGroupOp[ValidationResultNel[D]],
                                   f: (A,B,C,D) => Z)
    extends OptionalObjectFieldGroup[(A,B,C,D),Z] with FieldGroupOp[ValidationResultNel[Option[Z]]] {

    def tupledF = f.tupled

    def optional() = OptionalObj4(key, op1, op2, op3, op4, f)

    def extractChildren(jsonProducer: JsonProducer): ValidationResultNel[(A,B,C,D)] = {
      ( op1.extract(jsonProducer), op2.extract(jsonProducer), op3.extract(jsonProducer), op4.extract(jsonProducer)).mapN( (_,_,_,_) )
    }
  }


}

